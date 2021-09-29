{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
module Parser where

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
