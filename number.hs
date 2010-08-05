{-# LANGUAGE EmptyDataDecls #-}
module Number where

class Number n
data Zero
instance Number Zero 
data Succ n
instance (Number n) => Number (Succ n)
