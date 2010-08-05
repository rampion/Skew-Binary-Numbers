{-# LANGUAGE TypeFamilies,GADTs #-}
module Skew where
import Number

class Atom a where
  type Particle a
  fuse :: (Number n) => Particle a -> a n -> a n -> a (Succ n)
  decay :: (Number n) => a (Succ n) -> (Particle a, a n, a n)
  none :: a Zero

data IntAtom n where
  IntAtom :: (Number n) => IntAtom n

instance Atom IntAtom where
  type Particle IntAtom = ()
  fuse _ _ _ = IntAtom
  decay _ = ( (), IntAtom, IntAtom)
  none = IntAtom

data TreeAtom a n where
  Branch :: (Number n) => a -> TreeAtom a n -> TreeAtom a n -> TreeAtom a (Succ n)
  Leaf :: TreeAtom a Zero

instance Atom (TreeAtom a) where
  type Particle (TreeAtom a) = a
  fuse = Branch
  decay (Branch a l r) = (a, l, r)
  none = Leaf

class Element e where
  type BaseAtom e
  empty :: e -> Bool
  inject :: BaseAtom e -> e -> e
  extract :: e -> Maybe (BaseAtom e, e)

data IntElement n where
  IntElement :: (Number n) => Int -> IntElement n

instance Element (IntElement n) where
  type BaseAtom (IntElement n) = IntAtom n
  empty (IntElement c) = (c == 0)
  inject _ (IntElement c) = IntElement $ c + 1
  extract (IntElement c) = if (c > 0) then Just ( IntAtom, IntElement $ c - 1 ) else Nothing
