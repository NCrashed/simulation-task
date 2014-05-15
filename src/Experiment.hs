module Experiment(
    specs
  , experiment
  ) where
  
import Simulation.Aivika.Specs
import Simulation.Aivika.Generator
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.TimeSeriesView
import Data.Functor ((<$>))

specs :: Specs
specs = Specs { spcStartTime = 0.0, 
                spcStopTime = 100.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

genQueueStatistics :: String -> String -> [ExperimentGenerator]
genQueueStatistics prefix descrPostfix = 
        [ outputView $ defaultFinalStatsView 
          { finalStatsTitle = "Статистика очереди " ++ descrPostfix
          , finalStatsSeries = [prefix ++ "_wait"] 
          }
        , outputView $ defaultLastValueView
          { lastValueTitle = "Характеристики очереди " ++ descrPostfix
          , lastValueSeries = [prefix ++ "_extracted", prefix ++ "_rate", prefix ++ "_load"]
          }]

genQueueStatistics' :: String -> String -> [ExperimentGenerator]
genQueueStatistics' prefix descrPostfix = 
        [ outputView $ defaultFinalStatsView 
          { finalStatsTitle = "Статистика очереди " ++ descrPostfix
          , finalStatsSeries = [prefix ++ "_wait"] 
          }
        , outputView $ defaultLastValueView
          { lastValueTitle = "Характеристики очереди " ++ descrPostfix
          , lastValueSeries = [ prefix ++ "_extracted", prefix ++ "_rate"
                              , prefix ++ "1_load", prefix ++ "2_load"]
          }]
          
genQueueGraphics :: [String] -> String -> String -> [ExperimentGenerator]
genQueueGraphics series descr name = [
  outputView $ defaultTimeSeriesView 
        { timeSeriesTitle = descr
        , timeSeriesFileName = UniqueFileName (name ++ " - $RUN_INDEX") ".png"
        , timeSeries = Left <$> series
        }]
        
experiment :: Experiment
experiment = 
  defaultExperiment 
  { experimentSpecs = specs
  , experimentRunCount = 1
  , experimentTitle = "Домашнее задание по курсу 'Иммитационное моделирование'"
  , experimentDescription = ""
  , experimentGenerators = 
      [ outputView defaultExperimentSpecsView ] ++
      genQueueStatistics  "q1" "Стенда1" ++
      genQueueStatistics  "q2" "Стенда2" ++
      genQueueStatistics' "cq" "Станции" ++
      --genQueueStatistics "fq" "Наладчика" ++
      --genQueueStatistics "pq" "Упаковщика" ++
      genQueueGraphics ["q1", "q2"] "График состояния очередей стендов" "StandState" ++
      genQueueGraphics ["cq"] "График состояния очереди станции" "StationState" ++
      genQueueGraphics ["fq"] "График состояния очереди наладчика" "FixState" ++
      genQueueGraphics ["pq"] "График состояния очереди упаковщика" "PackState"
  }