{-# LANGUAGE TypeFamilies,GADTs,MultiParamTypeClasses,FunctionalDependencies,UndecidableInstances #-}
module Weight where
import Chemistry
import Control.Monad (guard)

data Weight where 
  Known :: Int -> Weight
  Unknown :: Weight
  deriving (Show, Eq, Ord)

wmap :: (Int -> Int) -> Weight -> Weight
wmap f (Known w) = Known (f w)
wmap f _ = Unknown

wmap2 :: (Int -> Int -> Int) -> Weight -> Weight -> Weight
wmap2 f (Known w) (Known w') = Known (f w w')
wmap2 _ _ _ = Unknown

instance Num Weight where
  (+) = wmap2 (+)
  (-) = wmap2 (-)
  (*) = wmap2 (*)
  negate = wmap negate
  abs = wmap abs
  signum = wmap signum
  fromInteger = Known . fromInteger

data Weighted a where
  Weighted :: Weight -> a -> Weighted a

weight :: Weighted a -> Weight
weight (Weighted w _) = w

wmerge :: Weight -> Weight -> Maybe Weight
wmerge Unknown Unknown = Nothing
wmerge w Unknown = Just w
wmerge Unknown w = Just w
wmerge w w' = if w == w' then Just w else Nothing

instance (Atom a) => Atom (Weighted a) where
  type Particle (Weighted a) = Particle a
  ether = Weighted 0 ether
  fuse p (Weighted w a) (Weighted w' b) = do
    w'' <- wmerge w w'
    c <- fuse p a b
    return $ Weighted (w''+1) c
  decay (Weighted w a) = do
    (p, l, r) <- decay a
    return (p, Weighted (w-1) l, Weighted (w-1) r)

instance (Subscript s a) => Subscript (Weighted s) (Weighted a) where
  zero = Weighted Unknown zero
  increment (Weighted w a) (Weighted w' s) = do
    w'' <- wmerge w w'
    s' <- increment a s
    return $ Weighted w'' s'
  decrement (Weighted w s) = do
    (a, s') <- decrement s
    return $ (Weighted w a, Weighted w s')
