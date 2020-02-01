module ExperimentEngine

import Control.ST
import Data.Vect

import Individual
import Population
import Vect

export
data MostSuccessfulIndividual : Type where
  MostSuccessful
    : (individual : Individual)
    -> (pop : Population popSize)
    -> MaxBy Individual.performance individual (individuals pop)
    -> MostSuccessfulIndividual

public export
data TerminationCountdown = ActionsRemaining Nat

public export
interface ExperimentEngine (m : Type -> Type) where
  Experiment : PopulationSize -> TerminationCountdown -> Type
  prepare
    : (popSize : PopulationSize)
    -> (countdown : TerminationCountdown)
    -> ST m Var [add (Experiment popSize countdown)]
  perform
    : (x : Var)
    -> ST m () [x ::: Experiment popSize countdown :-> Experiment popSize (ActionsRemaining Z)]
  conclude
    : (x : Var)
    -> ST m MostSuccessfulIndividual [remove x (Experiment popSize (ActionsRemaining Z))]

ExperimentEngine m where
  Experiment popSize countdown = State (Population popSize)
  prepare popSize countdown = new $ Population.generate popSize
  perform x = pure ()
  conclude x = do
    pop <- read x
    delete x
    let (champion ** championProof) = maxBy Individual.performance (individuals pop)
    pure $ MostSuccessful champion pop championProof
