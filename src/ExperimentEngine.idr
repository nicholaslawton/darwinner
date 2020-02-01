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
  step
    : (x : Var)
    -> ST m () [x ::: Experiment popSize (ActionsRemaining (S k))
                  :-> Experiment popSize (ActionsRemaining k)]
  conclude
    : (x : Var)
    -> ST m MostSuccessfulIndividual [remove x (Experiment popSize (ActionsRemaining Z))]

export
perform
  : ExperimentEngine m
  => (x : Var)
  -> ST m () [x ::: Experiment {m} popSize (ActionsRemaining countdown)
                :-> Experiment {m} popSize (ActionsRemaining Z)]
perform x {countdown = Z} = pure ()
perform x {countdown = S k} = do
  step x
  perform x

ExperimentEngine m where
  Experiment popSize countdown = State (Population popSize)
  prepare popSize countdown = new $ Population.generate popSize
  step = ?step
  conclude x = do
    pop <- read x
    delete x
    let (champion ** championProof) = maxBy Individual.performance (individuals pop)
    pure $ MostSuccessful champion pop championProof
