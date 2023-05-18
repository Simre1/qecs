module Data.HList where
import Data.Constraint

data HList as where
    HEmpty :: HList '[]
    HCons :: a -> HList as -> HList (a:as)

type family Contains a as :: Constraint where
    Contains a (a:as) = ()
    Contains a (b:as) = Contains a as