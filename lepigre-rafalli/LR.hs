{- Exploring Lepigre-Rafalli encoding to see if it would
   enable us to use affine lambda calculus only, for the
   core language.

   The short answer is that while the encoding itself is
   affine, to recurse with it appears to require non-linear
   functions.

   The definition of the encoding is ported from
   "Monotone recursive types and recursive data representations in Cedille" by
   Jenkins and Stump 2021.
-}

module LR where

import Prelude hiding (succ)


type NatRec x z s = z -> s -> z -> s -> x

data NatZ x = NatZ (forall z s . z -> s -> x)

data NatS x = NatS (forall z s . NatRec x z s -> z -> s -> x)

data Nat = Nat (forall x . NatRec x (NatZ x) (NatS x))

zero :: Nat
zero = Nat (\ z s z' s' -> case z of
                            NatZ u -> u z' s')

succ :: Nat -> Nat
succ (Nat n) = Nat (\ z s z' s' -> case s of
                         NatS u -> u (\ z' s' -> n z' s') z' s')

-- notice that while the encoding above is linear, and while it indeed supports recursive
-- computation like the following toInt function, one has to use non-linear terms to
-- define particular recursions
toInt :: Nat -> Int
toInt (Nat n) = n (NatZ (\ z s -> 0)) (NatS (\ r z s -> 1 + r z s z s)) (NatZ (\ z s -> 0)) (NatS (\ r z s -> 1 + r z s z s)) 