{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
module Co where

import           Control.Monad                  (unless, void, when)
import           Control.Monad.Base             (MonadBase)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Control.Monad.Cont             (ContT, MonadCont, callCC,
                                                 runContT)
import           Control.Monad.Except           (ExceptT, MonadError (..),
                                                 runExceptT)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.State.Strict     (MonadState, StateT, evalStateT)
import qualified Control.Monad.State.Strict     as State
import           Data.Foldable                  (for_, traverse_)
import           Data.IORef.Lifted
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Sequence                  (ViewL ((:<)), (|>))
import qualified Data.Sequence                  as Seq
import           Data.Void                      (Void)
import           System.IO                      (hPutStrLn, stderr)
import           Text.Megaparsec                hiding (runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Pretty.Simple             (CheckColorTty (..),
                                                 OutputOptions (..),
                                                 defaultOutputOptionsNoColor,
                                                 pPrintOpt)

data Expr
    = LNull
    | LBool Bool
    | LStr String
    | LNum Integer
    | Variable Identifier
    | Binary BinOp Expr Expr
    | Call Identifier [Expr]
    | Receive Expr
    deriving (Eq, Show)

type Identifier = String

data BinOp = Plus | Minus | Equals | NotEquals | LessThan | GreaterThan
    deriving (Eq, Show)

data Stmt
    = ExprStmt Expr
    | VarStmt Identifier Expr
    | AssignStmt Identifier Expr
    | IfStmt Expr [Stmt]
    | WhileStmt Expr [Stmt]
    | FunctionStmt Identifier [Identifier] [Stmt]
    | ReturnStmt (Maybe Expr)
    | YieldStmt
    | SpawnStmt Expr
    | SendStmt Expr Identifier
    deriving (Eq, Show)

type Program = [Stmt]

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

semi, identifier, stringLiteral :: Parser String
semi = symbol ";"
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)
stringLiteral = char '"' >> manyTill L.charLiteral (char '"') <* sc

integer :: Parser Integer
integer = lexeme (L.signed sc L.decimal)

runParser :: Parser a -> String -> Either String a
runParser parser code = do
  case parse parser "" code of
    Left err   -> Left $ errorBundlePretty err
    Right prog -> Right prog

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint =
  pPrintOpt CheckColorTty $
    defaultOutputOptionsNoColor
      { outputOptionsIndentAmount = 2,
        outputOptionsCompact = True,
        outputOptionsCompactParens = True
      }

operators :: [[Operator Parser Expr]]
operators =
  [ [Prefix $ Receive <$ symbol "<-"],
    [ binary Plus $ symbol "+",
      binary Minus $ try (symbol "-" <* notFollowedBy (char '>'))
    ],
    [ binary LessThan $ symbol "<",
      binary GreaterThan $ symbol ">"
    ],
    [ binary Equals $ symbol "==",
      binary NotEquals $ symbol "!="
    ]
  ]
  where
    binary op symP = InfixL $ Binary op <$ symP

term :: Parser Expr
term =
  LNull <$ symbol "null"
    <|> LBool True <$ symbol "true"
    <|> LBool False <$ symbol "false"
    <|> LStr <$> stringLiteral
    <|> LNum <$> integer
    <|> try (Call <$> identifier <*> parens (sepBy expr (char ',' *> sc)))
    <|> Variable <$> identifier
    <|> parens expr

expr :: Parser Expr
expr = makeExprParser term operators

stmt :: Parser Stmt
stmt =
  IfStmt <$> (symbol "if" *> parens expr) <*> braces (many stmt)
    <|> WhileStmt <$> (symbol "while" *> parens expr) <*> braces (many stmt)
    <|> VarStmt <$> (symbol "var" *> identifier) <*> (symbol "=" *> expr <* semi)
    <|> YieldStmt <$ symbol "yield" <* semi
    <|> SpawnStmt <$> (symbol "spawn" *> expr <* semi)
    <|> ReturnStmt <$> (symbol "return" *> optional expr <* semi)
    <|> FunctionStmt
      <$> (symbol "function" *> identifier)
      <*> parens (sepBy identifier (char ',' *> sc))
      <*> braces (many stmt)
    <|> try (AssignStmt <$> identifier <*> (symbol "=" *> expr <* semi))
    <|> try (SendStmt <$> expr <*> (symbol "->" *> identifier <* semi))
    <|> ExprStmt <$> expr <* semi

program :: Parser Program
program = sc *> many stmt <* eof

data Value
  = Null
  | Boolean Bool
  | Str String
  | Num Integer
  | Function Identifier [Identifier] [Stmt] Env

instance Show Value where
  show = \case
    Null                -> "null"
    Boolean b           -> show b
    Str s               -> s
    Num n               -> show n
    Function name _ _ _ -> "function " <> name

instance Eq Value where
  Null == Null             = True
  Boolean b1 == Boolean b2 = b1 == b2
  Str s1 == Str s2         = s1 == s2
  Num n1 == Num n2         = n1 == n2
  _ == _                   = False

newtype Interpreter a = Interpreter
  { runInterpreter ::
      ExceptT Exception (StateT InterpreterState IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadState InterpreterState, MonadError Exception)

type Env = Map.Map Identifier (IORef Value)

data InterpreterState = InterpreterState
  { isEnv        :: Env
  }

newInterpreterState :: IO InterpreterState
newInterpreterState = do
  coroutines <- newIORef Seq.empty
  return $ InterpreterState Map.empty

data Exception
  = Return Value
  | RuntimeError String

defineVar :: Identifier -> Value -> Interpreter ()
defineVar name value = do
  env <- State.gets isEnv
  env' <- defineVarEnv name value env
  setEnv env'

defineVarEnv :: Identifier -> Value -> Env -> Interpreter Env
defineVarEnv name value env = do
  valueRef <- newIORef value
  return $ Map.insert name valueRef env

setEnv :: Env -> Interpreter ()
setEnv env = State.modify' $ \is -> is { isEnv = env }

lookupVar :: Identifier -> Interpreter Value
lookupVar name =
  State.gets isEnv >>= findValueRef name >>= readIORef

assignVar :: Identifier -> Value -> Interpreter ()
assignVar name value =
  State.gets isEnv >>= findValueRef name >>= flip writeIORef value

findValueRef :: Identifier -> Env -> Interpreter (IORef Value)
findValueRef name env =
  case Map.lookup name env of
    Just ref -> return ref
    Nothing  -> throw $ "Unknown variable: " <> name

throw :: String -> Interpreter a
throw = throwError . RuntimeError

evaluate :: Expr -> Interpreter Value
evaluate = \case
  LNull            -> pure Null
  LBool b          -> pure $ Boolean b
  LStr s           -> pure $ Str s
  LNum n           -> pure $ Num n
  Variable name    -> lookupVar name
  binary@Binary {} -> evaluateBinaryOp binary
  call@Call {}     -> evaluateFuncCall call
  _                -> throw "Not implemented"

evaluateBinaryOp :: Expr -> Interpreter Value
evaluateBinaryOp ~(Binary op leftE rightE) = do
  left <- evaluate leftE
  right <- evaluate rightE
  let errMsg msg = msg <> ": " <> show left <> " and " <> show right
  case (op, left, right) of
    (Plus, Num n1, Num n2)        -> pure $ Num $ n1 + n2
    (Plus, Str s1, Str s2)        -> pure $ Str $ s1 <> s2
    (Plus, Str s1, _)             -> pure $ Str $ s1 <> show right
    (Plus, _, Str s2)             -> pure $ Str $ show left <> s2
    (Plus, _, _)                  -> throw $ errMsg "Cannot add or append"

    (Minus, Num n1, Num n2)       -> pure $ Num $ n1 - n2
    (Minus, _, _)                 -> throw $ errMsg "Cannot subtract"

    (LessThan, Num n1, Num n2)    -> pure $ Boolean $ n1 < n2
    (LessThan, _, _)              -> throw $ errMsg "Cannot compare non-numbers"
    (GreaterThan, Num n1, Num n2) -> pure $ Boolean $ n1 > n2
    (GreaterThan, _, _)           -> throw $ errMsg "Cannot compare non-numbers"

    (Equals, _, _)                -> pure $ Boolean $ left == right
    (NotEquals, _, _)             -> pure $ Boolean $ left /= right

execute :: Stmt -> Interpreter ()
execute = \case
  ExprStmt expr -> void $ evaluate expr
  VarStmt name expr -> evaluate expr >>= defineVar name
  AssignStmt name expr -> evaluate expr >>= assignVar name
  IfStmt expr body -> do
    cond <- evaluate expr
    when (isTruthy cond) $
      traverse_ execute body
  while@(WhileStmt expr body) -> do
    cond <- evaluate expr
    when (isTruthy cond) $ do
      traverse_ execute body
      execute while
  ReturnStmt mExpr -> do
    mRet <- traverse evaluate mExpr
    throwError . Return . fromMaybe Null $ mRet
  FunctionStmt name params body -> do
    env <- State.gets isEnv
    defineVar name $ Function name params body env
  where
    isTruthy = \case
      Null      -> False
      Boolean b -> b
      _         -> True


evaluateFuncCall :: Expr -> Interpreter Value
evaluateFuncCall ~(Call funcName argsEs) = case funcName of
  "print" -> executePrint argsEs
  funcName -> lookupVar funcName >>= \case
    func@Function {} -> evaluateFuncCall' func argsEs
    val              -> throw $ "Cannot call a non-function: " <> show val
  where
    executePrint = \case
      [expr] -> evaluate expr >>= liftIO . print >> return Null
      _      -> throw "print takes exactly one argument"

evaluateFuncCall' :: Value -> [Expr] -> Interpreter Value
evaluateFuncCall' ~func@(Function funcName params body funcDefEnv) argEs = do
  checkArgCount
  funcCallEnv <- State.gets isEnv
  setupFuncEnv
  retVal <- executeBody funcCallEnv
  setEnv funcCallEnv
  return retVal
  where
    checkArgCount = when (length argEs /= length params) $
      throw $ funcName <> " call expected " <> show (length params)
        <> " argument(s) but got " <> show (length argEs)

    setupFuncEnv = do
      args <- traverse evaluate argEs
      funcDefEnv' <- defineVarEnv funcName func funcDefEnv
      setEnv funcDefEnv'
      for_ (zip params args) $ uncurry defineVar

    executeBody funcCallEnv =
      (traverse_ execute body >> return Null) `catchError` \case
        Return retVal -> return retVal
        err           -> setEnv funcCallEnv >> throwError err

interpret :: Program -> IO (Either String ())
interpret program = do
  state <- newInterpreterState
  retVal <- flip evalStateT state
      . runExceptT
      . runInterpreter
      $ traverse_ execute program
  case retVal of
    Left (RuntimeError err) -> return $ Left err
    Left (Return _) -> return $ Left "Cannot return for outside functions"
    Right _ -> return $ Right ()


runFile :: FilePath -> IO ()
runFile file = do
  code <- readFile file
  case runParser program code of
    Left err -> hPutStrLn stderr err
    Right program -> interpret program >>= \case
      Left err -> hPutStrLn stderr $ "ERROR: " <> err
      _        -> return ()
