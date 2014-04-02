{-# LANGUAGE DoAndIfThenElse #-}
module Model(
    model
  ) where

import Stand

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Simulation
import Simulation.Aivika.Ref
import Simulation.Aivika.Event
import Simulation.Aivika.Experiment
import Simulation.Aivika.Parameter

model :: Simulation ExperimentData
model = do
  totalUpTime <- newRef 0.0
  
--  pid1 <- newProcessId
--  pid2 <- newProcessId
--  
--  let machine :: Process ()
--      machine = do
--        startUpTime <- liftDynamics time
--        upTime <- liftIO $ exprnd upRate
--        holdProcess upTime
--        finishUpTime <- liftDynamics time
--        liftEvent $
--          modifyRef totalUpTime
--          (+ (finishUpTime - startUpTime))
--        repairTime <- liftIO $ exprnd repairRate
--        holdProcess repairTime
--        machine
  runStand =<< standOne
  runStand =<< standTwo
  
  let getUptime :: Dynamics Double
      getUptime = do
        upTime <- runEvent $ readRef totalUpTime
        totalTime <- liftParameter stoptime
        let k = totalTime / (2 * upTime)
        if k > 1.0 then return 0.0/0.0
        else return k
        
  experimentDataInStartTime 
    [ ("t", seriesEntity "time" time)
    , ("ut", seriesEntity "UpTime" getUptime)]  
    