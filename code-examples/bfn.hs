-- | A polymorphic n-ary tree data structure.

data Tree a = T a [Tree a]
  deriving Show

-- | Function that annotates each node of a tree with the order in which
-- | a BFS traversal would visit it.

bfn :: Tree a -> Tree (a, Int)
bfn t = t'
  where (t', ks') = aux ks t
        ks = 1 : ks'  -- the magic knot!!!
        aux (k : ks) (T x ts) = (T (x, k) ts', (k+1) : ks')
          where (ts', ks') = auxs ks ts
        auxs ks [] = ([], ks)
        auxs ks (t : ts) = (t' : ts', ks'')
          where (t', ks') = aux ks t
                (ts', ks'') = auxs ks' ts

-- | Example

t = T 'a' [ T 'b' []
          , T 'c' [ T 'e' []
                  , T 'f' []
                  ]
          , T 'd' []
          ]

main = print (bfn t)
