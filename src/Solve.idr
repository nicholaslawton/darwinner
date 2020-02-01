module Solve

import Control.ST

import ExperimentEngine

solve : ExperimentEngine m => ST m MostSuccessfulIndividual []
solve = do
  experiment <- prepare
  perform experiment
  conclude experiment
