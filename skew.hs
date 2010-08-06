module Skew where
import Chemistry
import Weight
import Data.Maybe (fromJust)

data ListPrefix a = ListPrefix [a] Int

dec :: (Atom a, Subscript s a) => ListPrefix (Weighted s) -> Maybe (Particle a, ListPrefix (Weighted s))
dec l = Just (undefined, l)

inc :: (Atom a, Subscript s a) => Particle a -> ListPrefix (Weighted s) -> Maybe (ListPrefix (Weighted s))
inc _ l = Just l

normalize :: (Subscript s a) => ListPrefix (Weighted s) -> ListPrefix (Weighted s)
normalize unnormalized = normalized
  where (normalized, queue) = normalize' (ListPrefix queue 0) unnormalized
        normalize' :: (Subscript s a) => ListPrefix (Weighted s) -> ListPrefix (Weighted s) -> ( ListPrefix (Weighted s), [Weighted s])
        normalize' prefix (ListPrefix _ 0) = (prefix, [])
        normalize' prefix@(ListPrefix qs m) terms@(ListPrefix (s:ss) n) =
          let count = cardinality s 
              tail = ListPrefix ss (n-1)
          in
          if count == 0 then 
            normalize' prefix tail
          else if count <= (if m == 0 then 2 else 1) then
            let prefix' = ListPrefix qs (m+1)
                (normalized, suffix) = normalize' prefix' tail
            in (normalized, s:suffix)
          else if m > 0 {- and count >= 2 -} then fromJust $ do
            (p, prefix') <- dec prefix
            terms' <- inc p terms
            return $ normalize' prefix' terms'
          else {- if m == 0 and count > 2 -} fromJust $ do
            (p, ListPrefix (s':ss') n') <- dec terms
            terms' <- inc p (ListPrefix ss' (n' - 1))
            return $ normalize' prefix terms'

