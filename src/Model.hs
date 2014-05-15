{-# LANGUAGE DoAndIfThenElse #-}
module Model(
    model
  ) where

import Computer
import Stand
import Controller
import Fixer
import Packer

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
  transportQueue <- newFCFSQueue 10
  (Packer packQueue packWorkRef _ packFunc) <- packer transportQueue 12
  (Fixer  fixQueue fixWorkRef fixFunc) <- fixer packQueue
  stationQueue <- newFCFSQueue 200 
  (Controller contrWorkRef1 contr1Func) <- controller fixQueue packQueue stationQueue
  (Controller contrWorkRef2 contr2Func) <- controller fixQueue packQueue stationQueue
  (Stand queueOne workRef1 stand1Func) <- standOne stationQueue
  (Stand queueTwo workRef2 stand2Func) <- standTwo stationQueue
  
  runProcessInStartTime $ computerStream queueOne queueTwo
  mapM_ runProcessInStartTime [ stand1Func
                              , stand2Func
                              , contr1Func
                              , contr2Func
                              , fixFunc
                              , packFunc ] 
                              
  let getLoadFactor :: Ref Double -> Dynamics Double
      getLoadFactor ref = do
        workTime <- runEvent $ readRef ref
        start <- liftParameter starttime
        end <- liftParameter stoptime
        return (workTime / (end - start)) 
  
  let genQueueExperimentData :: String -> String -> FCFSQueue a -> [(String, SeriesEntity)]
      genQueueExperimentData prefix descrPostfix queue = 
        [ (prefix, seriesEntity ("Размер очереди " ++ descrPostfix) (runEvent $ queueCount queue))
        , (prefix ++ "_wait", seriesEntity ("Время ожидания в очереди " ++ descrPostfix) (runEvent $ queueWaitTime queue))
        , (prefix ++ "_extracted", seriesEntity ("Кол-во обработанных компьютеров " ++ descrPostfix) (runEvent $ dequeueExtractCount queue))  
        , (prefix ++ "_rate", seriesEntity ("Скорость пополнения очереди " ++ descrPostfix) (runEvent $ enqueueRate queue))]
  
  let genExperimentLoadData :: String -> String -> Ref Double -> [(String, SeriesEntity)]
      genExperimentLoadData prefix descPostfix ref = 
        [(prefix ++ "_load", seriesEntity ("Коэффициент загрузки " ++ descPostfix) (getLoadFactor ref))]
        
  experimentDataInStartTime $
    [ ("t", seriesEntity "Модельное время" time) ] ++
    genQueueExperimentData "q1" "Стенда1" queueOne ++ 
    genQueueExperimentData "q2" "Стенда2" queueTwo ++ 
    genExperimentLoadData  "q1" "Стенда1" workRef1 ++
    genExperimentLoadData  "q2" "Стенда2" workRef2 ++
    
    genQueueExperimentData "cq" "Станции" stationQueue ++
    genExperimentLoadData  "cq1" "Контроллера1" contrWorkRef1 ++
    genExperimentLoadData  "cq2" "Контроллера2" contrWorkRef2 ++
    
    genQueueExperimentData "fq" "Наладчика" fixQueue ++
    genQueueExperimentData "pq" "Упаковщика" packQueue ++
    genExperimentLoadData  "fq" "Наладчика" fixWorkRef ++
    genExperimentLoadData  "pq" "Упаковщика" packWorkRef ++
    
    genQueueExperimentData "tq" "Транспортера" transportQueue
