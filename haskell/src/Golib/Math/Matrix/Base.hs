module Golib.Math.Matrix.Base
       (Trans (..)) where

data Trans = NoTrans | Trans deriving (Show, Enum) -- NOTE: Documentation of Enum says the enums are assumed to be numerated
                                                   -- from 0 to n-1, so this should do.
