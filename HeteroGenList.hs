{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HList where


import           Data.Kind                      ( Constraint
                                                , Type
                                                )



data HList (ts :: [Type]) where
    HNil ::HList '[]
    (:#) ::t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

hTail :: HList (t ': ts) -> HList ts
hTail (_ :# ts) = ts

-- instance Eq (HList '[]) where
--     HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--     (a :# as) == (b :# bs) = a == a && as == bs

type family AllEq (ts :: [Type]) :: Constraint where
    AllEq '[] = ()
    AllEq (t ': ts) = (Eq t, AllEq ts)


type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
    HNil      == HNil      = True
    (a :# as) == (b :# bs) = a == a && as == bs
