#!/usr/bin/runghc
-- excerpt adapted from: "What I Wish I Knew When I Learned Haskell", by Stephen Diehl
-- Higher Order Abstract Syntax (HOAS) Interpreter
-- Higher Order Abstract Syntax (HOAS) is a technique for implementing the lambda calculus in a language where the binders of the lambda expression map directly onto lambda binders of the host language ( i.e. Haskell ) to give us substitution machinery in our custom language by exploiting Haskell's implementation.
{-# LANGUAGE GADTs #-}

data Expr a where
  Con :: a -> Expr a
  Lam :: (Expr a -> Expr b) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b

i :: Expr (a -> a)
i = Lam (\x -> x)

k :: Expr (a -> b -> a)
k = Lam (\x -> Lam (\y -> x))

s :: Expr ((a -> b -> c) -> (a -> b) -> (a -> c))
s = Lam (\x -> Lam (\y -> Lam (\z -> App (App x z) (App y z))))

eval :: Expr a -> a
eval (Con v) = v
eval (Lam f) = \x -> eval (f (Con x))
eval (App e1 e2) = (eval e1) (eval e2)


skk :: Expr (a -> a)
skk = App (App s k) k

example :: Integer
example = eval skk 1
-- 1

main :: IO ()
main = putStrLn $ show example
