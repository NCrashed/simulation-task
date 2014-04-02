module Main where

import Model
import Experiment

import Simulation.Aivika.Experiment
  
main :: IO ()
main = runExperiment experiment model