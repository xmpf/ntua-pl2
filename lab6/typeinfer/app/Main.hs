{-# OPTIONS_GHC -O2 #-}

import Text.Read      -- read
import Data.Maybe     -- maybe

-- datatypes
data Type = Tvar Int 
          | Tfun Type Type
          deriving Eq

data Expr = Evar String
          | Eabs String Expr
          | Eapp Expr Expr
          deriving Eq

-- type synonyms
type Constraint = (Type, Type)
type Constraints = [Constraint]
type Env = [(String, Int)]

-- Pretty printing of expressions
always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions
instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types
instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Populate Constraints
mkConstraints :: Expr -> Env -> Int -> Maybe (Type, Constraints, Int)
mkConstraints (Evar str) env var = do
    lookup str env >>= \t -> Just (Tvar t, [], var)
mkConstraints (Eabs str expr) env var = do
    mkConstraints expr ((str, var') : env) var' >>=
      \(a, b, c) -> Just ((Tfun (Tvar var') a), b, 1 + c)
      where var' = 1 + var
mkConstraints (Eapp expr1 expr2) env var = do
    mkConstraints expr1 env var >>=
      \(a, b, c) -> mkConstraints expr2 env c >>=
      \(d, e, f) -> Just (Tvar (f + 1), (a, Tfun d (Tvar (f + 1))) : (b ++ e), f + 1)

-- lookup a type in a type
tLookup :: Type -> Type -> Bool
tLookup t1 t2@(Tvar a) = t1 == t2
tLookup t1 t2@(Tfun t' t'') = (tLookup t1 t') || (tLookup t1 t'')

-- replace a type in type
tReplace :: Type -> Type -> Constraints -> Constraints -> Constraints
tReplace _ _ [] c' = c'
tReplace ta tb ((ta', tb') : c) c' = tReplace ta tb c ((ta'', tb'') : c')
  where ta'' = replace ta tb ta'
        tb'' = replace ta tb tb'
        replace a b c@(Tvar d) = case (a == c) of
                                  True -> b
                                  False -> c
        replace a b c@(Tfun e' e'') = Tfun (replace a b e') (replace a b e'')

-- unify W algorithm
unify :: Constraints -> Constraints -> Maybe Constraints
unify [] r = Just r
unify ((ta, tb) : tc) r | (ta == tb) = unify tc r
unify ((ta@(Tvar a), tb) : tc) r | not (tLookup ta tb) = unify (tReplace ta tb tc []) ((ta, tb) : r)
unify ((ta, tb@(Tvar b)) : tc) r | not (tLookup tb ta) = unify (tReplace tb ta tc []) ((tb, ta) : r)
unify ((ta@(Tfun ta' ta''), tb@(Tfun tb' tb'')) : tc) r = unify ((ta', tb') : (ta'', tb'') : tc) r
unify _ _ = Nothing

-- substitute
sub1 :: Type -> Constraints -> Type
sub1 t@(Tfun ta tb) r = Tfun (sub1 ta r) (sub1 tb r)
sub1 ta@(Tvar a) r =
    case lookup ta r of
        Just t' -> replace ta t' ta
        Nothing -> ta
    where replace :: Type -> Type -> Type -> Type
          replace a b c@(Tvar d) = case (a == c) of
                                      True -> b
                                      False -> c
          replace a b c@(Tfun e' e'') = Tfun (replace a b e') (replace a b e'')

sub' :: Type -> Constraints -> Maybe Type
sub' t r = do
    x <- pure $ sub1 t r 
    if t == x then Just t 
    else sub' x r


-- enumerate types 'lexicographically'
enumTypes :: Type -> Maybe Type
enumTypes t = Just (sub1 t (fst sorted_types))
    where sorted_types = enumList t 0 []
          enumList t@(Tvar a) counter subs =
            case lookup t subs of
                Just t -> (subs, counter)
                Nothing -> (((t, Tvar counter) : subs), counter + 1)
          enumList t@(Tfun t1 t2) counter subs =
            let (subs1, counter1) = enumList t1 counter subs
                (subs2, counter2) = enumList t2 counter1 subs1
            in (subs2, counter2)

-- Main program
typeInfer = do
    s <- getLine
    let e = read s :: Expr
    let type' = mkConstraints e [] 0 >>=
              \(t, c, _) -> unify c [] >>=
              sub' t >>= enumTypes
    maybe (putStrLn "type error") print type'

count n m  =  sequence $ take n $ repeat m

main :: IO ()
main = do
  n <- readLn
  count n typeInfer >> return ()
