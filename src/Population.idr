module Population

import Data.Vect

import Individual

public export
PopulationSize : Type
PopulationSize = Nat

export
Population : PopulationSize -> Performance -> Type

export
individuals : Population (S popSize) wins -> Vect (S popSize) Individual

export
generate : (popSize : PopulationSize) -> popSize = S k -> Population (S k) (Wins Z)

export
recordWin
  : Elem winner (individuals pop)
  -> (pop : Population (S popSize) (Wins wins))
  -> Population (S popSize) (Wins (S wins))
