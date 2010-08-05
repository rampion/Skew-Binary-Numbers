{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,UndecidableInstances #-}
module Skew where

data Leaf a = Leaf a
data Branch b a = Branch { left :: b a, value :: a, right :: b a }

class FullBinaryTree t where
  height :: t a -> Int
  size :: t a -> Int

instance FullBinaryTree Leaf where
  height _ = 1
  size _ = 1

instance (FullBinaryTree t) => FullBinaryTree (Branch t) where
  height t = 1 + (height $ left t)
  size t = 1 + 2 * (size $ left $ t)


data AscendingList a b = a :<=: b
class IsSmallerThan a b
instance (IsSmallerThan a b, IsSmallerThan b c) => IsSmallerThan a c
instance IsSmallerThan a ()
instance (FullBinaryTree t, IsSmallerThan (Branch t a) b) => IsSmallerThan (t a) (AscendingList (Branch t a) b)
