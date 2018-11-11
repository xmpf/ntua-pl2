-- A purely functional queue, using two lists
data Queue a = Q [a] [a]

empty :: Queue a
empty = Q [] []

isEmpty :: Queue a -> Bool
isEmpty (Q l r) = null l && null r

appendRight :: Queue a -> a -> Queue a
appendRight (Q l r) x = Q l (x : r)

popLeft :: Queue a -> (a, Queue a)
popLeft (Q (x : xs) r) = (x, Q xs r)
popLeft (Q [] []) = error "cannot pop"
popLeft (Q [] r) = popLeft (Q (reverse r) [])
