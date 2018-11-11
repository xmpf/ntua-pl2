data Expr = Con Integer
          | Rnd [Integer]
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Catch Expr Expr
          | Tick

eval :: Expr -> MS Integer Integer
eval (Con n) =
  return n
eval (Add x y) = do
  vx <- eval x
  vy <- eval y
  return $ vx + vy
eval (Sub x y) =  do
  vx <- eval x
  vy <- eval y
  return $ vx - vy
eval (Mul x y) =  do
  vx <- eval x
  vy <- eval y
  return $ vx * vy
eval (Div x y) =  do
  vx <- eval x
  vy <- eval y
  if vy /= 0 then
    return $ vx `div` vy
  else
    fail "division by zero"
eval (Mod x y) =  do
  vx <- eval x
  vy <- eval y
  if vy /= 0 then
    return $ vx `mod` vy
  else
    fail "division by zero"
{-
eval (Catch x y) =
  eval x `catchE` eval y
eval (Rnd ns) =
  Nondet ns
-}
eval Tick = tick

e1 = (Con 17 `Add` Con 25) `Mod` Con 15
e2 = Con 7 `Add` (Con 17 `Div` Con 0)
e3 = ((Con 7 `Add` e2) `Catch` Con 0) `Add` Con 6
e4 = Con 7 `Add` Rnd [3, 5, 8]
e5 = Tick `Add` (Tick `Add` (Tick `Mul` Tick))

data MS s a = St (s -> (a, s))

instance Monad (MS s) where
  return x    =  St $ \s -> (x, s)
  St h >>= f  =  St $ \s -> let (a, s') = h s
                                St k    = f a
                            in  k s'

tick :: MS Integer Integer
tick = St $ \s -> (s, s+1)

runS :: MS s a -> s -> a
runS (St h) s = fst $ h s

data ML a = Nondet [a]
  deriving Show

instance Monad ML where
  return x = Nondet [x]
  Nondet l >>= f = Nondet $ concat [l' | x <- l, let Nondet l' = f x]

data ME a = Value a | Error String
  deriving Show

instance Monad ME where
  return = Value
  Value x   >>= f  =  f x
  Error msg >>= f  =  Error msg
  fail = Error

catchE :: ME a -> ME a -> ME a
catchE (Value vx) y = return vx
catchE (Error msg) y = y

instance Show Expr where
  showsPrec p (Con n)     = showsPrec p n
  showsPrec p (Rnd xs)    = showsPrec 0 xs
  showsPrec p (Add x y)   = showParen (p > 4) $
                            showsPrec 4 x . (" + " ++) . showsPrec 5 y
  showsPrec p (Sub x y)   = showParen (p > 4) $
                            showsPrec 4 x . (" - " ++) . showsPrec 5 y
  showsPrec p (Mul x y)   = showParen (p > 5) $
                            showsPrec 5 x . (" * " ++) . showsPrec 6 y
  showsPrec p (Div x y)   = showParen (p > 5) $
                            showsPrec 5 x . (" / " ++) . showsPrec 6 y
  showsPrec p (Mod x y)   = showParen (p > 5) $
                            showsPrec 5 x . (" % " ++) . showsPrec 6 y
  showsPrec p (Catch x y) = showParen (p > 2) $
                            ("try " ++) . showsPrec 0 x . (" catch " ++) . showsPrec 2 y
  showsPrec p Tick        = ("tick" ++)

instance Read Expr where
  readsPrec p s =
    (readParen False $ \s ->
      [(Con n, r) | (n, r) <- readsPrec 0 s]) s ++
    (readParen (p > 4) $ \s ->
       [(Add x y, r) | (x, t1)   <- readsPrec 5 s,
                       ("+", t2) <- lex t1,
                       (y, r)    <- readsPrec 5 t2]) s ++
    (readParen (p > 4) $ \s ->
       [(Sub x y, r) | (x, t1)   <- readsPrec 5 s,
                       ("-", t2) <- lex t1,
                       (y, r)    <- readsPrec 5 t2]) s ++
    (readParen (p > 5) $ \s ->
       [(Mul x y, r) | (x, t1)   <- readsPrec 6 s,
                       ("*", t2) <- lex t1,
                       (y, r)    <- readsPrec 6 t2]) s ++
    (readParen (p > 5) $ \s ->
       [(Div x y, r) | (x, t1)   <- readsPrec 6 s,
                       ("/", t2) <- lex t1,
                       (y, r)    <- readsPrec 6 t2]) s ++
    (readParen (p > 5) $ \s ->
       [(Mod x y, r) | (x, t1)   <- readsPrec 6 s,
                       ("%", t2) <- lex t1,
                       (y, r)    <- readsPrec 6 t2]) s
{-  
  readsPrec p (Catch x y) = readParen (p > 2) $
                            ("try " ++) . readsPrec 0 x . (" catch " ++) . readsPrec 2 y
-}
