module Solve

import Control.ST

import ExperimentEngine
import Population

solve
  : ExperimentEngine m
  => PopulationSize
  -> TerminationCountdown
  -> ST m MostSuccessfulIndividual []
solve popSize countdown = do
  experiment <- prepare popSize countdown
  perform experiment
  conclude experiment
