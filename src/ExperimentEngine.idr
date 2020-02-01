module ExperimentEngine

import Control.ST

export
MostSuccessfulIndividual : Type

public export
data ExperimentState
  = Running
  | Complete

public export
interface ExperimentEngine (m : Type -> Type) where
  Experiment : ExperimentState -> Type
  prepare : ST m Var [add (Experiment Running)]
  perform : (x : Var) -> ST m () [x ::: Experiment Running :-> Experiment Complete]
  conclude : (x : Var) -> ST m MostSuccessfulIndividual [remove x (Experiment Complete)]
