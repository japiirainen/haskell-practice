{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module OpenSum where


import           Data.Kind
import           Data.Proxy
import           Fcf
import           GHC.TypeLits            hiding ( type (+) )
import           Unsafe.Coerce


data OpenSum (f :: k -> Type) (ts :: [k]) where
    UnsafeOpenSum ::Int -> f t -> OpenSum f ts


type FindElem (key :: k) (ts :: [k]) =
    FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inject :: forall f t ts. Member t ts => f t -> OpenSum f ts
inject = UnsafeOpenSum (findElem @t @ts)

project :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
project (UnsafeOpenSum i f) =
    if i == findElem @t @ts
        then Just $ unsafeCoerce f
        else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ unsafeCoerce (n - 1) t

weaken :: OpenSum f ts -> OpenSum f (t ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t