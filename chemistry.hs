{-# LANGUAGE TypeFamilies,GADTs,MultiParamTypeClasses,FunctionalDependencies #-}
module Chemistry where

class Atom a where
  type Particle a
  ether :: a
  fuse :: Particle a -> a -> a -> Maybe a
  decay :: a -> Maybe (Particle a, a, a)

encapsulate :: (Atom a) => Particle a -> Maybe a
encapsulate p = fuse p ether ether

decapsulate :: (Atom a) => a -> Maybe (Particle a)
decapsulate a = do
  (p, _, _) <- decay a
  return p

data CountAtom where
  CountAtom :: CountAtom

instance Atom CountAtom where
  type Particle CountAtom = ()
  ether = CountAtom
  fuse _ _ _ = Just CountAtom
  decay _ = Just ( (), CountAtom, CountAtom )

data TreeAtom p where
  Branch :: p -> TreeAtom p -> TreeAtom p -> TreeAtom p
  Leaf :: TreeAtom p

instance Atom (TreeAtom p) where
  type Particle (TreeAtom p) = p
  ether = Leaf
  fuse p l r = Just $ Branch p l r 
  decay (Branch p l r) = Just (p, l, r)
  decay _ = Nothing

class (Atom a) => Subscript s a | s -> a where
  zero :: s
  include :: a -> s -> Maybe s
  extract :: s -> Maybe (a, s)
  cardinality :: s -> Int

data CountSubscript where
  CountSubscript :: Int -> CountSubscript

instance Subscript CountSubscript CountAtom where
  zero = CountSubscript 0
  include _ (CountSubscript c) = Just $ CountSubscript $ c + 1
  extract (CountSubscript 0) = Nothing
  extract (CountSubscript i) = Just ( CountAtom, CountSubscript $ i - 1 )
  cardinality (CountSubscript i) = i

data TreeSubscript a where
  TreeSubscript :: [ TreeAtom a ] -> TreeSubscript a

instance Subscript (TreeSubscript a) (TreeAtom a) where
  zero = TreeSubscript []
  include a (TreeSubscript as) = Just $ TreeSubscript (a:as)
  extract (TreeSubscript []) = Nothing
  extract (TreeSubscript (a:as)) = Just (a, TreeSubscript as)
  cardinality (TreeSubscript as) = length as

