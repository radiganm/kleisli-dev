-- Main.hs (for essence-p001)
-- from Philip Wadler's publication "The essence of functional programming" (1992)
-- adapted by Mac Radigan

module Main where

import Lib
import Numeric

-- Interpreter:
type Name                           = String

data Term                           = Var  Name
                                    | Con  Int
                                    | Add  Term Term
                                    | Lam  Name Term
                                    | App  Term Term

data Value                          = Wrong
                                    | Num Int
                                    | Fun (Value -> M Value)

type Environment                    = [(Name, Value)]

showval                             :: Value -> String

showval Wrong                       = "<wrong>"

showval (Num i)                     = show i

showval (Fun f)                     = "<functional>"

interp                              :: Term -> Environment -> M Value
interp (Var x) e                    = mylookup x e
interp (Con i) e                    = unitM (Num i)
interp (Add u v) e                  = interp u e `bindM` (\a ->
                                      interp v e `bindM` (\b ->
                                      add a b))
interp (Lam x v) e                  = unitM (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e                  = interp t e `bindM` (\f ->
                                      interp u e `bindM` (\a ->
                                      apply f a))

mylookup                            :: Name -> Environment -> M Value
mylookup x []                       = unitM Wrong
mylookup x ((y,b):e)                = if x==y then unitM b else mylookup x e

add                                 :: Value -> Value -> M Value
add (Num i) (Num j)                 = unitM (Num (i+j))
add a b                             = unitM Wrong

apply                               :: Value -> Value -> M Value
apply (Fun k) a                     = k a 
apply f a                           = unitM Wrong

test                                :: Term -> String
test t                              = showM (interp t [])

-- Test Data:
--
-- NB. term0 := ((\x.x+x)(10+11))
term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 11)))

-- Trivial Monad (I)
type I a     = a

unitI a      = a
a `bindI` k  = k a
showI a      = showval a

-- Substitute Monad (M)
type M a     = a

unitM a      = a
a `bindM` k  = k a
showM a      = showval a

-- Main
main :: IO ()
main = do
  print $ test term0

-- *EOF*
