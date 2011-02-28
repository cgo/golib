module Golib.Math.Base
       (Trans (..),
        Index) where

data Trans = NoTrans | Trans deriving (Show, Enum) -- NOTE: Documentation of Enum says the enums are assumed to be numerated
                                                   -- from 0 to n-1, so this should do.

type Index = Int
