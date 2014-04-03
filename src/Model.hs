{-# LANGUAGE DoAndIfThenElse #-}
module Model(
    model
  ) where

import Computer
import Stand

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.Experiment
import Simulation.Aivika.Process
import Simulation.Aivika.Queue

model :: Simulation ExperimentData
model = do
  stOne@(Stand queueOne _ _) <- standOne
  stTwo@(Stand queueTwo _ _) <- standTwo
  
  runProcessInStartTime $ computerStream queueOne queueTwo
  runStand stOne
  runStand stTwo 
        
  experimentDataInStartTime 
    [ ("t", seriesEntity "time" time)
    , ("q1", seriesEntity "Queue1 size" (runEvent $ queueCount queueOne))
    , ("q2", seriesEntity "Queue2 size" (runEvent $ queueCount queueTwo))]  
