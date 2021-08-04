{-# LANGUAGE LambdaCase #-}
module Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char

-- Base parser

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser p s =
    case parse p s of
        [(res, [])] -> res
        [(_, _)]    -> error "Parser did not consume entire stream"
        _           -> error "Parser error"


item :: Parser Char
item = Parser $ \case
        []     -> []
        (c:cs) -> [(c, cs)]

unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]

instance Functor Parser where
    fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
    (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s
    return a = Parser $ \s -> [(a, s)]

instance MonadPlus Parser where
    mzero = failure
    mplus = combine

instance Alternative Parser where
    empty = mzero
    (<|>) = option


combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> parse p s <> parse q s

failure :: Parser a
failure = Parser $ const []

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of
        []  -> parse q s
        res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
    if p c
    then unit c
    else failure


-- Combinators
