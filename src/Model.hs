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
import Simulation.Aivika.Parameter
import Simulation.Aivika.Ref
import Simulation.Aivika.Queue

model :: Simulation ExperimentData
model = do
  (Stand queueOne workRef1 stand1Func) <- standOne
  (Stand queueTwo workRef2 stand2Func) <- standTwo
  
  runProcessInStartTime $ computerStream queueOne queueTwo
  runProcessInStartTime stand1Func
  runProcessInStartTime stand2Func 
  
  let getLoadFactor :: Ref Double -> Dynamics Double
      getLoadFactor ref = do
        workTime <- runEvent $ readRef ref
        start <- liftParameter starttime
        end <- liftParameter stoptime
        return (workTime / (end - start)) 
        
  experimentDataInStartTime 
    [ ("t", seriesEntity "Модельное время" time)
    , ("q1", seriesEntity "Размер очереди Стенда1" (runEvent $ queueCount queueOne))
    , ("q2", seriesEntity "Размер очереди Стенда2" (runEvent $ queueCount queueTwo))
    , ("q1_wait", seriesEntity "Время ожидания в очереди" (runEvent $ queueWaitTime queueOne))
    , ("q2_wait", seriesEntity "Время ожидания в очереди" (runEvent $ queueWaitTime queueTwo))
    , ("q1_extracted", seriesEntity "Кол-во обработанных компьютеров" (runEvent $ dequeueExtractCount queueOne))  
    , ("q2_extracted", seriesEntity "Кол-во обработанных компьютеров" (runEvent $ dequeueExtractCount queueTwo))
    , ("q1_rate", seriesEntity "Скорость пополнения" (runEvent $ enqueueRate queueOne))
    , ("q2_rate", seriesEntity "Скорость пополнения" (runEvent $ enqueueRate queueTwo))
    , ("q1_load", seriesEntity "Коэффициент загрузки" (getLoadFactor workRef1))
    , ("q2_load", seriesEntity "Коэффициент загрузки" (getLoadFactor workRef2))]  
