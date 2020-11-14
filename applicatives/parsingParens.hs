{-# LANGUAGE DeriveFunctor #-}
module ParsingParens where

import Control.Applicative (Applicative (..))
import qualified Data.List as List

newtype Parser a = Parser (String -> Maybe (String, a))
    deriving Functor

instance Applicative Parser where
    pure x = Parser (\str -> Just (str, x))
    liftA2 = parserLiftA2

parserLiftA2 :: (a -> b -> c)
                -> Parser a -> Parser b
                -> Parser c
parserLiftA2 f (Parser p1) (Parser p2) =
    Parser $ \str -> 
        do
          (str' ,x) <- p1 str
          (str'', y) <- p2 str'
          Just (str'', f x y)

main :: IO ()
main = print "this is a greate parser"


