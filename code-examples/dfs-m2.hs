import Control.Monad.State (State, evalState, get, modify)

data Tree a = Node a [Tree a]
  deriving Show

t = Node 'a' [ Node 'b' []
             , Node 'c' [ Node 'f' []
                        , Node 'g' []
                        ]
             , Node 'd' []
             , Node 'e' [ Node 'h' []
                        ]
             ]
    
dfsM :: Tree a -> Tree (a, Int)
dfsM = flip evalState 1 . aux
  where aux :: Tree a -> State Int (Tree (a, Int))
        aux (Node x ts) = do id <- get
                             modify (\s -> id+1)
                             ts' <- auxs ts
                             return $ Node (x, id) ts'
        auxs :: [Tree a] -> State Int [Tree (a, Int)]
        auxs [] = return []
        auxs (t : ts) = do t' <- aux t
                           ts' <- auxs ts
                           return $ t' : ts'

main = do
  putStrLn "Warming up with the state monad..."
  let s :: Int
      s = 1
  let x = flip evalState s $ do one <- get
                                modify (+ 1)
                                two <- get
                                modify (* 3)
                                six <- get
                                modify (\n -> n * (n+1))
                                result <- get
                                return (one, two, six, result)
  print x
  putStrLn "Original tree:"
  print t
  putStrLn "Tree annotated with DFS numbering:"
  print $ dfsM t
