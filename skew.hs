{-# LANGUAGE TypeFamilies,GADTs #-}
module Skew where
import Number

class Atom a where
  type Value a
  fuse :: (Number n) => Value a -> a n -> a n -> a (Succ n)
  decay :: (Number n) => a (Succ n) -> (Value a, a n, a n)
  none :: a Zero

data IntAtom n = IntAtom

instance Atom IntAtom where
  type Value IntAtom = ()
  fuse _ _ _ = IntAtom
  decay _ = ( (), IntAtom, IntAtom)
  none = IntAtom

data TreeAtom a n where
  Branch :: (Number n) => a -> TreeAtom a n -> TreeAtom a n -> TreeAtom a (Succ n)
  Leaf :: TreeAtom a Zero

instance Atom (TreeAtom a) where
  type Value (TreeAtom a) = a
  fuse = Branch
  decay (Branch a l r) = (a, l, r)
  none = Leaf
