{-# LANGUAGE TypeFamilies,GADTs,MultiParamTypeClasses,FlexibleInstances,UndecidableInstances #-}
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
  type BaseAtom e :: * -> *
  empty :: e n -> Bool
  inject :: BaseAtom e n -> e n -> e n
  extract :: e n -> Maybe (BaseAtom e n, e n)

data IntElement n where
  IntElement :: (Number n) => Int -> IntElement n

instance Element IntElement where
  type BaseAtom IntElement = IntAtom
  empty (IntElement c) = (c == 0)
  inject _ (IntElement c) = IntElement $ c + 1
  extract (IntElement 0) = Nothing
  extract (IntElement i) = Just ( IntAtom, IntElement $ i - 1 )

data TreeElement a n where
  TreeElement :: (Number n) => [ TreeAtom a n ] -> TreeElement a n

instance Element (TreeElement a) where
  type BaseAtom (TreeElement a) = TreeAtom a
  empty (TreeElement as) = null as
  inject a (TreeElement as) = TreeElement (a:as)
  extract (TreeElement []) = Nothing
  extract (TreeElement (a:as)) = Just (a, TreeElement as)

class LessThan a b
instance (LessThan a b, LessThan b c) => LessThan a c
instance (Number n) => LessThan n (Succ n)
--instance (Atom a, Number n) => LessThan (a n) (a (Succ n))
instance (Element e, Number n) => LessThan (e n) (e (Succ n))

class Ordered o where
  type First o
  type Rest o

