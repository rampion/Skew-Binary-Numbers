{-# LANGUAGE TypeFamilies,GADTs #-}
module Skew where
import Control.Monad (guard)

type Weight = Int

class Atom a where
  type Particle a
  ether :: a
  fuse :: Particle a -> a -> a -> Maybe a
  decay :: a -> Maybe (Particle a, a, a)

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

data Weighted a where
  Weighted :: (Atom a) => Weight -> a -> Weighted a

instance (Atom a) => Atom (Weighted a) where
  type Particle (Weighted a) = Particle a
  ether = Weighted 0 ether
  fuse p (Weighted w a) (Weighted v b) = do
    guard $ w == v
    c <- fuse p a b
    return $ Weighted (w+1) c
  decay (Weighted w a) = do
    (p, l, r) <- decay a
    return (p, Weighted (w-1) l, Weighted (w-1) r)

