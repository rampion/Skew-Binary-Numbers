{-# LANGUAGE FlexibleInstances,TypeSynonymInstances #-}
module Skew where
import Chemistry
import Weight
import Control.Monad (guard)

data ListPrefix a = ListPrefix [a] Int
  deriving (Eq)

type CountList = ListPrefix (Weighted CountSubscript)
instance Show CountList where
  show (ListPrefix l n) = s
    where (_, s) = foldl showDigits (0, "o") $ take n l
          showDigits (m, s) x@(Weighted (Known w) (CountSubscript i)) 
            | m + 1 < w = showDigits (m+1, '0':s) x
            | otherwise = (w, (if 0 <= i && i <= 9 then show i else "(" ++ show i ++ ")") ++ s)

digit :: String -> Maybe (Int, String)
digit [] = Nothing
digit s@(d:s')  | '0' <= d && d <= '9' = Just (read [d], s')
                | otherwise = if null ri then Nothing else Just $ head ri
                    where ri = readParen True reads s

digits :: [Int] -> ReadS [Int]
digits is s = maybe [(is, s)] (\(i,s) -> digits (i:is) s) (digit s)

instance Read CountList where
  readsPrec _ s = do
    (is, 'o':s) <- digits [] s
    let l = map (\(w,i) -> Weighted (Known w) (CountSubscript i)) $
            filter ((0 /=) . snd) $ 
            zip [1,2..] is
    let n = length l
    return ( ListPrefix l n, s )
  
dec :: (Atom a, Subscript s a) => ListPrefix (Weighted s) -> 
       Maybe (Particle a, ListPrefix (Weighted s))
dec (ListPrefix _ 0) = Nothing
dec (ListPrefix (s:ss) n) = do
  -- remove one of the smallest atoms
  (a, s) <- extract s
  -- decay the atom into a particle and two lighter atoms
  (p, l, r) <- decay a
  -- make a collection of the lighter atoms
  s'<- (include l zero >>= include r)
  -- include the original colection if it's nonempty
  let (prepend, i) = if 0 > cardinality s then ((s:), 1) else (id, 0)
  -- include the new collection if it's not weightless
  let (prepend', i') = if 0 > weight s' then ((s':), 1) else (id, 0)
  return (p, ListPrefix (prepend' $ prepend ss) (n - 1 + i + i'))

inc :: (Atom a, Subscript s a) => Particle a -> ListPrefix (Weighted s) -> Maybe (ListPrefix (Weighted s))
inc p (ListPrefix ss n) 
  | n == 0 || cardinality (head ss) == 1 = do
    -- create an atom out of the particle
    a <- encapsulate p
    -- use the weight 1 collection if available
    let (s, ss', n') = if n == 0 || weight (head ss) > 1
                       then (zero, ss, n)
                       else (head ss, tail ss, n - 1)
    -- pack the new atom into its collection
    s <- include a s
    return $ ListPrefix (s:ss') (1+n')
  | otherwise = do
    let s:ss' = ss
    -- remove two atoms from the collection
    (l, s) <- extract s
    (r, s) <- extract s
    -- fuse them with the particle to create a heavier atom
    a <- fuse p l r
    -- include the original collection if it's nonempty
    let (prepend, i) = if 0 < cardinality s then ((s:), 1) else (id, 0)
    -- create or find a collection for the heavier atom
    let (s', ss'', n'') = if n == 1 || weight (head ss') > 1 + weight s 
                          then (zero, ss', n - 1)
                          else (head ss', tail ss', n - 2)
    -- pack the heavier atom in its collection
    s' <- include a s'
    return $ ListPrefix (prepend $ s':ss'') (i+1+n'')
        
normalize :: (Subscript s a) => ListPrefix (Weighted s) -> Maybe (ListPrefix (Weighted s))
normalize unnormalized = normalized
  where (normalized, queue) = normalize' (ListPrefix queue 0) unnormalized
        normalize' :: (Subscript s a) => ListPrefix (Weighted s) -> 
                      ListPrefix (Weighted s) -> ( Maybe (ListPrefix (Weighted s)), [Weighted s])
        -- stop normalizing when we run out of terms
        normalize' prefix (ListPrefix _ 0) = (Just prefix, [])
        normalize' prefix@(ListPrefix qs m) terms@(ListPrefix (s:ss) n) =
          let count = cardinality s 
              tail = ListPrefix ss (n-1) in
          -- elide items with count zero
          if count == 0 then
            normalize' prefix tail
          -- allow all 0s and 1s (and 2s if they are the first nonzero)
          else if count <= (if m == 0 then 2 else 1) then
            let (normalized, suffix) = normalize' (ListPrefix qs $ m+1) tail
            in (normalized, s:suffix)
          else 
            maybe (Nothing, []) (uncurry normalize') $
              -- if not the first nonozero, decrement the seen half,
              -- and increment the reset to get rid of a two
              if m > 0 {- and count >= 2 -} then do
                (p, prefix') <- dec prefix
                terms' <- inc p terms
                return (prefix', terms')
              -- if more than two (and no nonzeroes), decrement the unseen half,
              -- then increment to get rid of a two
              else {- if m == 0 and count > 2 -} do
                (p, ListPrefix (s':ss') n') <- dec terms
                terms' <- inc p (ListPrefix ss' (n' - 1))
                return (prefix, terms')
