{-# LANGUAGE DoAndIfThenElse #-}
module Model(
    model
  ) where

import Computer
import Stand
import Controller
import Fixer
import Packer
import Transporter
import Parameters

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
  storeQueue <- newFCFSQueue 100
  (Transporter transportQueue transportWorkRef transportFunc) <- transporter storeQueue
  (Packer packQueue packWorkRef _ packFunc) <- packer transportQueue
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
                              , packFunc
                              , transportFunc ] 
                              
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
  
  let unbox (a, b) = [a, b]
        
  experimentDataInStartTime $
    [ ("t", seriesEntity "Модельное время" time) ] ++

    genQueueExperimentData "q1" "Стенда1" queueOne ++ 
    genExperimentLoadData  "q1" "Стенда1" workRef1 ++

    genQueueExperimentData "q2" "Стенда2" queueTwo ++ 
    genExperimentLoadData  "q2" "Стенда2" workRef2 ++
    
    genQueueExperimentData "cq" "Станции" stationQueue ++
    genExperimentLoadData  "cq1" "Контроллера1" contrWorkRef1 ++
    genExperimentLoadData  "cq2" "Контроллера2" contrWorkRef2 ++
    
    genQueueExperimentData "fq" "Наладчика" fixQueue ++
    genExperimentLoadData  "fq" "Наладчика" fixWorkRef ++
    
    genQueueExperimentData "pq" "Упаковщика" packQueue ++
    genExperimentLoadData  "pq" "Упаковщика" packWorkRef ++
    
    genQueueExperimentData "tq" "Транспортера" transportQueue ++
    genExperimentLoadData  "tq" "Транспортера" transportWorkRef ++
    
    genQueueExperimentData "sq" "Склада" storeQueue ++
    
    [ ("computer1Rate", seriesEntity "Доля компьютеров первого вида" (return computer1Rate :: Dynamics Double))
    , ("computer1Stand1Time", seriesEntity "Время обработки компьютеров первого вида на первом стенде" (return computer1Stand1Time :: Dynamics Double))
    , ("computer2Stand1Time", seriesEntity "Время обработки компьютеров второго вида на первом стенде" (return computer2Stand1Time :: Dynamics Double))
    , ("computer2Stand2Time", seriesEntity "Время обработки компьютеров второго вида на втором стенде" (return computer2Stand2Time :: Dynamics Double))
    , ("computerGeneratorIntense", seriesEntity "Интенсивность потока компьютеров" (return computerGeneratorIntense :: Dynamics Double))
    , ("controllerTimeDistr", seriesEntity "Распределение времени проверки на контроллере" (return (unbox controllerTimeDistr) :: Dynamics [Double]))
    , ("controllerSuccessRate", seriesEntity "Вероятность успешного прохождения теста на контроллере" (return controllerSuccessRate :: Dynamics Double))
    , ("fixTimeDist", seriesEntity "Распределение времени наладки компьютера" (return (unbox fixTimeDist) :: Dynamics [Double]))
    , ("paleteMaxSize",  seriesEntity "Размер палеты" (return paleteMaxSize :: Dynamics Int))
    , ("packTimeDistr", seriesEntity "Распределение времени упаковки компьютера" (return (unbox packTimeDistr) :: Dynamics [Double]))
    , ("transportTimeDistr", seriesEntity "Распределение времени транспортировки палеты" (return (unbox transportTimeDistr) :: Dynamics [Double]))
    ]
