module Vect

import Data.Vect

export
data MaxBy : Ord b => (a -> b) -> a -> Vect (S k) a -> Type where
  MaxBySingle : Ord b => {p : a -> b} -> MaxBy p x [x]
  MaxByTail : Ord b => {p : a -> b} -> MaxBy p x xs -> compare (p x) (p y) = GT -> MaxBy p x (y :: xs)
  MaxByHead : Ord b => {p : a -> b} -> MaxBy p x xs -> Not (compare (p x) (p y) = GT) -> MaxBy p y (y :: xs)

Uninhabited (LT = EQ) where
  uninhabited Refl impossible

Uninhabited (with Prelude.Interfaces (LT = GT)) where
  uninhabited Refl impossible

Uninhabited (EQ = GT) where
  uninhabited Refl impossible

DecEq Ordering where
  decEq LT LT = Yes Refl
  decEq LT EQ = No absurd
  decEq LT GT = No absurd
  decEq EQ LT = No (negEqSym absurd)
  decEq EQ EQ = Yes Refl
  decEq EQ GT = No absurd
  decEq GT LT = No (negEqSym absurd)
  decEq GT EQ = No (negEqSym absurd)
  decEq GT GT = Yes Refl

export
maxBy : Ord b => (p : a -> b) -> (xs : Vect (S k) a) -> (x : a ** MaxBy p x xs)
maxBy {k = Z} p [x] = (x ** MaxBySingle)
maxBy {k = S j} p (y :: xs) =
  let (x ** xMaxTail) = maxBy p xs
  in case decEq (compare (p x) (p y)) GT of
    Yes xGreater => (x ** MaxByTail xMaxTail xGreater)
    No xNotGreater => (y ** MaxByHead xMaxTail xNotGreater)
