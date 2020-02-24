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
    -> (pop : Population (S popSize) wins)
    -> MaxBy Individual.performance individual (individuals pop)
    -> MostSuccessfulIndividual

public export
data TerminationCountdown = ActionsRemaining Nat

public export
interface ExperimentEngine (m : Type -> Type) where
  Experiment : PopulationSize -> Performance -> TerminationCountdown -> Type

  prepare
    : (popSize : PopulationSize)
    -> popSize = S k
    -> (countdown : TerminationCountdown)
    -> ST m Var [add (Experiment (S k) Z countdown)]

  step
    : (x : Var)
    -> ST m () [x ::: Experiment (S popSize) perf (ActionsRemaining (S k))
                  :-> Experiment (S popSize) (S perf) (ActionsRemaining k)]

  conclude
    : (x : Var)
    -> ST m MostSuccessfulIndividual [remove x (Experiment (S popSize) perf (ActionsRemaining Z))]

export
perform
  : ExperimentEngine m
  => (x : Var)
  -> ST m () [x ::: Experiment {m} (S popSize) perf (ActionsRemaining countdown)
                :-> Experiment {m} (S popSize) (countdown + perf) (ActionsRemaining Z)]
perform x {countdown = Z} = pure ()
perform x {countdown = S k} {perf} = do
  step x
  (rewrite plusSuccRightSucc k perf in perform x)

ExperimentEngine m where
  Experiment popSize perf countdown = State (Population popSize perf)

  prepare popSize popSizeNotZero countdown = new $ Population.generate popSize popSizeNotZero

  step x = (do
    pop <- read x
    let (_ ** winner) = takeFirst (individuals pop)
    write x $ recordWin winner pop)
      where
        takeFirst : (xs : Vect (S k) elem) -> (first : elem ** Elem first xs)
        takeFirst (first :: rest) = (first ** Here)

  conclude x = do
    pop <- read x
    delete x
    let (champion ** championProof) = maxBy Individual.performance (individuals pop)
    pure $ MostSuccessful champion pop championProof
