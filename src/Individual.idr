module Individual

public export
data Performance = Wins Nat

Eq Performance where
  (==) (Wins x) (Wins y) = (==) x y

export
Ord Performance where
  compare (Wins x) (Wins y) = compare x y

export
Individual : Type

export
performance : Individual -> Performance
