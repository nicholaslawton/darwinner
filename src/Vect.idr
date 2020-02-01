module Vect

import Data.Vect

export
data MaxBy : Ord b => (a -> b) -> a -> Vect n a -> Type where
  MaxBySingle : Ord b => {p : a -> b} -> MaxBy p x [x]
  MaxByHead : Ord b => {p : a -> b} -> MaxBy p x xs -> compare (p y) (p x) = GT -> MaxBy p y (y :: xs)

export
maxBy : Ord b => (p : a -> b) -> (xs : Vect n a) -> (x : a ** MaxBy p x xs)
