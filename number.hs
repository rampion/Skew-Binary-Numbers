{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses,FlexibleInstances,UndecidableInstances #-}
module Number where

class Number n
data Zero
instance Number Zero 
data Succ n
instance (Number n) => Number (Succ n)
type One = Succ Zero

data Infinity

class (Number a) => LessThan a b
instance (Number n) => LessThan Zero (Succ n)
instance (Number n) => LessThan n Infinity
instance (LessThan a b, Number a, Number b) => LessThan (Succ a) (Succ b)
