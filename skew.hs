module Skew where
import Chemistry
import Weight
import Data.Maybe (fromJust)

data ListPrefix a = ListPrefix [a] Int

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
    let (s, ss', n') = if weight (head ss) == 1
                       then (head ss, tail ss, n - 1)
                       else (zero, ss, n)
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
    let (prepend, i) = if 0 > cardinality s then ((s:), 1) else (id, 0)
    -- create or find a collection for the heavier atom
    let (s', ss'', n'') = if weight (head ss') > 1 + weight s 
                          then (zero, ss', n - 1)
                          else (head ss', tail ss', n - 2)
    -- pack the heavier atom in its collection
    s' <- include a s'
    return $ ListPrefix (prepend $ s':ss'') (i+1+n'')
        
        
      

{-
normalize :: (Subscript s a) => ListPrefix (Weighted s) -> Maybe (ListPrefix (Weighted s))
normalize unnormalized = normalized
  where (normalized, queue) = normalize' (ListPrefix queue 0) unnormalized
        normalize' :: (Subscript s a) => ListPrefix (Weighted s) -> 
                      ListPrefix (Weighted s) -> ( ListPrefix (Weighted s), [Weighted s])
        normalize' prefix (ListPrefix _ 0) = (Just prefix, [])
        normalize' prefix@(ListPrefix qs m) terms@(ListPrefix (s:ss) n) =
          let count = cardinality s 
              tail = ListPrefix ss (n-1)
              (prefix', terms') = 
                if count == 0 then
                  (prefix, tail)
                else if count <= (if m == 0 then 2 else 1) then
                  (ListPrefix qs (m+1), tail)
                else if m > 0 {- and count >= 2 -} then fromJust $ do
                  (p, prefix') <- dec prefix
                  terms' <- inc p terms
                  return (prefix', terms')
                else {- if m == 0 and count > 2 -} fromJust $ do
                  (p, ListPrefix (s':ss') n') <- dec terms
                  terms' <- inc p (ListPrefix ss' (n' - 1))
                  return (prefix, terms')
          in
          normalize prefix' terms'
          -}
