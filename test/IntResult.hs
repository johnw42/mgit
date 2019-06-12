-- Demo of making a varargs-like function.

{-# LANGUAGE FlexibleInstances #-}
module IntResult (main) where
import Test.QuickCheck

-- The user interface.  Behaves like a function that takes zero or
-- more arguments and bundles them into a list.
ints :: IntResult r => r
ints = consIntResult []

class IntResult a where
  consIntResult :: [Int] -> a

instance IntResult [Int] where
  consIntResult = reverse

instance IntResult r => IntResult (Int -> r) where
  consIntResult xs x = consIntResult (x:xs)

prop_int0 = label "int0" $ once $ ints === ([] :: [Int])
prop_int1 a = label "int1" $ ints a === ([a] :: [Int])
prop_int2 a b = label "int2" $ ints a b === ([a, b] :: [Int])
prop_int3 a b c = label "int3" $ ints a b c === ([a, b, c] :: [Int])

main = do
  quickCheck prop_int0
  quickCheck prop_int1
  quickCheck prop_int2
  quickCheck prop_int3
