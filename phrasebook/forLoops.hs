module ForLoops where

import Data.Foldable
import Data.Traversable
import Control.Monad

main =
    do
        putStr "Numbers:"
        for_ [1..10] $ \i ->
          do
            putStr " "
            putStr (show i)
        putStr "\n"
        putStr "Odds:"

        for_ [1..10] $ \i ->
            when (odd i) $
                do
                    putStr " "
                    putStr $ show i
        putStr "\n"

        putStr "Odds2:"
        for_ (filter odd [1..10]) $ \i ->
            do
                putStr " "
                putStr $ show i
        putStr "\n"

        tens <- 
            for [1..5] $ \i ->
                do
                    putStr $ show i ++ " "
                    return $ i * 10
        putStr ("(sum: " ++ show (sum tens) ++ ")\n")
