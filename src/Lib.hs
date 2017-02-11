{-# LANGUAGE DeriveFunctor #-}
module Lib where

import           Data.Int
import           Prelude
data ExprF r = Const Int
             | Add r r
             | Mul r r
    deriving Functor

-- Fx :: f (Fix f) -> Fix f
-- Fixed point of a functor
newtype Fix f = Fx (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

type Expr = Fix ExprF

-- Algebras
type Algebra f a = f a -> a

type SimpleA = Algebra ExprF Int

type ExprInitAlg = Algebra ExprF (Fix ExprF)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

alg :: ExprF Int -> Int
alg (Const i)   = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

eval :: Fix ExprF -> Int
eval = cata alg

-- Examples


val :: Fix ExprF
val = Fx (Const 12)

testExpr :: Fix ExprF
testExpr = Fx $ (Fx $ (Fx $ Const 2) `Add` (Fx $ Const 3)) `Mul` (Fx $ Const 4)
