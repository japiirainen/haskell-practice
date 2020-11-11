{-# LANGUAGE OverloadedStrings #-}

import Lib

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

pairId :: Property
pairId =
    property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        ys <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        fmap id (Pair xs ys) === id (Pair xs ys)

pairComp :: Property
pairComp =
    property $ do
        x <- forAll $ Gen.integral (Range.linear 0 100)
        y <- forAll $ Gen.integral (Range.linear 0 100)
        composedNum (Pair x y) === fcomposedNum (Pair x y)

backwardId :: Property
backwardId =
    property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        ys <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        fmap id (BackwardPair xs ys) === id (BackwardPair xs ys)

backwardComp :: Property
backwardComp =
    property $ do
        x <- forAll $ Gen.integral (Range.linear 0 100)
        y <- forAll $ Gen.integral (Range.linear 0 100)
        composedNum (BackwardPair x y) === fcomposedNum (BackwardPair x y)

incPairId :: Property
incPairId = 
    property $ do
        x <- forAll $ Gen.integral (Range.linear 0 100) 
        y <- forAll $ Gen.integral (Range.linear 0 100)
        fmap id (IncrementPair x y) === id (IncrementPair x y)

incPairComp :: Property
incPairComp =
    property $ do 
        x <- forAll $ Gen.integral (Range.linear 0 100)
        y <- forAll $ Gen.integral (Range.linear 0 100)
        composedNum (IncrementPair x y) === fcomposedNum (IncrementPair x y)

tests :: IO ()
tests = do
        checkParallel $ Group "Pair Functor Tests" [
            ("pairId", pairId),
            ("pairComp", pairComp) 
                                                   ]
        checkParallel $ Group "BackwardPair Functor Tests" [
            ("backwardId", backwardId),
            ("backwardComp", backwardComp)
                                                           ]
        checkParallel $ Group "IncrementPair Functor Tests" [
            ("incPairId", incPairId),
            ("incPairComp", incPairComp)
                                                            ]
        return ()

main :: IO ()
main = putStrLn "Test suite not yet implemented"
