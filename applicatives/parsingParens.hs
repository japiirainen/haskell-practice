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

parserLiftA3 :: (a -> b -> c -> d)
        -> Parser a -> Parser b -> Parser c
        -> Parser d
parserLiftA3 f (Parser p1) (Parser p2) (Parser p3) =
    Parser $ \str -> 
        do
          (str', x) <- p1 str
          (str'', y) <- p2 str'
          (str''', z) <- p3 str''
          Just (str''', f x y z)


liftA3 :: Applicative f => (a -> b -> c -> d)
        -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

liftA4 :: Applicative f => (a -> b -> c -> d -> e)
        -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

parserMaybe :: Parser a -> String -> Maybe a
parserMaybe (Parser p) str =
    case p str of
      Just ([], x) -> Just x
      Just _       -> Nothing
      Nothing      -> Nothing

exact :: String -> Parser String
exact x = Parser $ \str ->
    case List.stripPrefix x str of
      Just str' -> Just (str', x)
      Nothing   -> Nothing

anythingBut :: Char -> Parser String
anythingBut c = Parser $ \str ->
    let (match, remainder) = List.span (/= c) str
     in Just (remainder, match)

main :: IO ()
main = print "this is a great parser"


