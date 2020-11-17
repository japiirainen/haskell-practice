{-# LANGUAGE InstanceSigs #-}
module ReaderContext where

import Control.Applicative

-- Reader
newtype Reader env a =
    Reader { runReader :: env -> a }

instance Functor (Reader env) where
    fmap :: (a -> b) -> Reader env a -> Reader env b
    fmap f (Reader x) = Reader (f . x)

instance Applicative (Reader e) where
    pure :: a -> Reader env a
    pure a = Reader (const a)
    liftA2 :: (a -> b -> c) -> Reader env a -> Reader env b -> Reader env c
    liftA2 f (Reader g) (Reader h) = Reader (\env -> f (g env) (h env))
    (<*>) :: Reader env (a -> b) -> Reader env a -> Reader env b
    (<*>) (Reader f) (Reader g) = Reader (\env -> f env $ g env) 

functionPure :: a -> w -> a
functionPure = const

functionLiftA2 :: (a -> b -> c) -> (w -> a) -> (w -> b) -> w -> c
functionLiftA2 f g h x = f (g x) (h x)

functionAp :: (w -> a -> b) -> (w -> a) -> w -> b
functionAp f g h = f h $ g h

assembleMessage :: String -> String -> String -> String
assembleMessage salutation pitch closing =
    salutation <> "\n\n" <> pitch <> "\n\n" <> closing

genericSalutation :: String
genericSalutation = "To whom it may concern:"

genericPitch :: String
genericPitch = "I have been looking at your work and I have an \
    \opportunity in the Bay Area that seems right up your alley."

genericClosing :: String
genericClosing = "- Recruiter"

genericMessage :: String
genericMessage = assembleMessage
    genericSalutation genericPitch genericClosing

personalizedSalutaion :: String -> String
personalizedSalutaion name = "Hello " <> name

personalizedPitch :: String -> String
personalizedPitch name = "I have been looking at your work, " <> name <>
        ", and I have an opportunity in the Bay Area that seems right up your alley."

personalizedMessage :: String -> String
personalizedMessage name = assembleMessage
    (personalizedSalutaion name)
    (personalizedPitch name)
    genericClosing

personalizedMessage' :: String -> String
personalizedMessage' = 
    (assembleMessage <$> personalizedSalutaion)
        <*> personalizedPitch
        <*> pure genericClosing

readerSalutation :: Reader String String
readerSalutation = Reader (\name -> "Hello " <> name <> ",")

readerPitch :: Reader String String
readerPitch = Reader (\name -> "I have been looking at your work, " <> name <>
        ", and I have an opportunity in the Bay Area that seems right up your alley.")

readerMessage :: Reader String String
readerMessage =
    (assembleMessage <$> readerSalutation)
        <*> readerPitch
        <*> pure genericClosing


messageToJoona = runReader readerMessage "Joona"


main :: IO ()
main = do
    putStrLn messageToJoona 

