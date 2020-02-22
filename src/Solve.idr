module Solve

import Control.ST

import ExperimentEngine
import Population
import Individual

solve
  : ExperimentEngine m
  => (popSize : PopulationSize)
  -> popSize = S k
  -> TerminationCountdown
  -> ST m MostSuccessfulIndividual []
solve popSize popSizeNotZero countdown@(ActionsRemaining k) = do
  experiment <- prepare popSize popSizeNotZero countdown
  perform experiment
  conclude experiment
