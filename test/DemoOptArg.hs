-- Demo of making a varargs-like function.

{-# LANGUAGE FlexibleInstances #-}
module Main (main) where
import Test.QuickCheck

-- Types for clarity--not needed for implementation.
newtype Mandatory a = Mandatory a
newtype Optional a = Optional a

-- Function with one mandatory argument and one optional argument.
optArg :: OptArgFunc a => Mandatory Int -> a
optArg x = optArg' x

class OptArgFunc a where
    optArg' :: Mandatory Int -> a

instance OptArgFunc [Int] where
    optArg' (Mandatory x) = [x]

instance OptArgFunc (Optional Int -> [Int]) where
    optArg' (Mandatory x) (Optional y) = [x, y]

prop_optArg1 x = label "optArg1" $ optArg (Mandatory x) == [x]
prop_optArg2 x y = label "optArg2" $ optArg (Mandatory x) (Optional y) == [x, y]

main = do
  quickCheck prop_optArg1
  quickCheck prop_optArg2
