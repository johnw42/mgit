-- Demo of making a varargs-like function.

{-# LANGUAGE FlexibleInstances #-}
module Main (main) where
import Test.QuickCheck

-- A vararg function.  Takes 0 or more arguments and combines them
-- into a list.
ints :: IntResult r => r
ints = ints' []

class IntResult a where
  ints' :: [Int] -> a

instance IntResult [Int] where
  ints' = reverse

instance IntResult r => IntResult (Int -> r) where
  ints' xs x = ints' (x:xs)

prop_int0 = label "int0" $ once $ ints === ([] :: [Int])
prop_int1 a = label "int1" $ ints a === ([a] :: [Int])
prop_int2 a b = label "int2" $ ints a b === ([a, b] :: [Int])
prop_int3 a b c = label "int3" $ ints a b c === ([a, b, c] :: [Int])

-- A polymorphic vararg function.  Takes 0 or more arguments and
-- combines them into a list.
mkList :: MkListResult r => r
mkList = mkList' []

class MkListResult r where
  mkList' :: [ListElem] -> r

instance MkListResult [ListElem] where
  mkList' = reverse

instance (ToListElem a, MkListResult r) => MkListResult (a -> r) where
  mkList' xs x = mkList' (toListElem x : xs)

-- Wrapper for arguments of mkList.
data ListElem = IntElem Int
              | CharElem Char
              | BoolElem Bool
              deriving (Eq, Show)

-- Class of possible arguments to mkList.
class ToListElem a where
    toListElem :: a -> ListElem

instance ToListElem Int where
    toListElem = IntElem

instance ToListElem Char where
    toListElem = CharElem

instance ToListElem Bool where
    toListElem = BoolElem

prop_mkList0 = label "mkList0" $ once $ mkList === ([] :: [ListElem])
prop_mkList1 a = label "mkList1" $ mkList a === [IntElem a]
prop_mkList2 a b = label "mkList2" $ mkList a b === [IntElem a, CharElem b]
prop_mkList3 a b c =
    label "mkList3" $
    mkList a b c === [IntElem a, CharElem b, IntElem c]

main = do
  quickCheck prop_int0
  quickCheck prop_int1
  quickCheck prop_int2
  quickCheck prop_int3
  quickCheck prop_mkList0
  quickCheck prop_mkList1
  quickCheck prop_mkList2
  quickCheck prop_mkList3
