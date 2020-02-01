module Population

import Data.Vect

import Individual

export
PopulationSize : Type

export
numberOfIndividuals : PopulationSize -> Nat

export
Population : PopulationSize -> Type

export
individuals : Population popSize -> Vect (numberOfIndividuals popSize) Individual

export
generate : (popSize : PopulationSize) -> Population popSize
