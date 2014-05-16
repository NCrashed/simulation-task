module Experiment(
    specs
  , experiment
  ) where
  
import Simulation.Aivika.Specs
import Simulation.Aivika.Generator
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.TimeSeriesView
import Data.Functor ((<$>))

taskText :: String
taskText = "Собранные компьюетры проходят серию испытаний на стендах технического контроля."
  ++ " Cтендовые испытания выполняются на двух испытательных стендах. На стендовые испытания"
  ++ " поступают компьютеры двух видов, соответственно 70% и 30%. Компьютеры 1-ого типа проходят"
  ++ " испытания на стенде 1, а компьютеры 2-ого типа - на стенде 1 и 2. Компьютеры типа 2 поступают"
  ++ " на стенда 1 в случае, если занят стенд 2. Компьютеры, поступающие на стендовые испытания, образуют"
  ++ " простейший поток с интенсивностью 0.5 изд./мин. Врем испытания компьютера имеет экспоненциальное распределение."
  ++ " Математическое ожидание времени обслуживания компьютера 1-ого типа на 1-м стенде - 7 мин, компьютера типа 2 на 2-м"
  ++ " стенде - 6 мин, а на 1-м стенде - 10 мин. \n\n"
  ++ " На последней станции у всех проверяют регулировку установки монитора. На станции находятся два контроллера."
  ++ " Каждому из них требуется на проверку время, равномерно распределенное в интервале [6, 12] мин. Примерно 85%"
  ++ " компьютеров проходят проверку успешно и попадают на участок упаковки. Остальные 15% попадают на участок наладки,"
  ++ " в котором находится один наладчик. Наладка занимает время, равномерно распределенное в интервале [20, 40] мин."
  ++ " На участке упаковки упаковочный робот размещает компьютеры на палету по 12 шт, затрачивая [1+-0.5] мин на каждый."
  ++ " Транспортный робот забирает одну заполненную палету, траспортирует ее на склад и возвращается назад, затрачивая"
  ++ " [16 +- 4] минут (распределено по равномернуому закону). \n\n"
  ++ " Построить модель для анализа работы указанного подразделения производственной линии. Определить состояние очередей"
  ++ " к стендам и роботам и коэффициент их загрузки."

specs :: Specs
specs = Specs { spcStartTime = 0.0, 
                spcStopTime = 200.0,
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

genParameters :: [ExperimentGenerator]
genParameters = [
  outputView $ defaultLastValueView
  { lastValueTitle = "Исходные параметры модели"
  , lastValueSeries = [ "computer1Rate"
                      , "computer1Stand1Time"
                      , "computer2Stand1Time"
                      , "computer2Stand2Time"
                      , "computerGeneratorIntense"
                      , "controllerTimeDistr"
                      , "controllerSuccessRate"
                      , "fixTimeDist"
                      , "paleteMaxSize"
                      , "packTimeDistr"
                      , "transportTimeDistr"]
  }]
  
experiment :: Experiment
experiment = 
  defaultExperiment 
  { experimentSpecs = specs
  , experimentRunCount = 1
  , experimentTitle = "Домашнее задание по курсу 'Иммитационное моделирование'"
  , experimentDescription = taskText
  , experimentGenerators = 
      [ outputView defaultExperimentSpecsView ] ++
      genParameters ++
      genQueueStatistics  "q1" "Стенда1" ++
      genQueueStatistics  "q2" "Стенда2" ++
      genQueueStatistics' "cq" "Станции" ++
      genQueueStatistics  "fq" "Наладчика" ++
      genQueueStatistics  "pq" "Упаковщика" ++
      genQueueStatistics  "tq" "Транспортера" ++
      genQueueGraphics ["q1", "q2"] "График состояния очередей стендов" "StandState" ++
      genQueueGraphics ["cq"] "График состояния очереди станции" "StationState" ++
      genQueueGraphics ["fq"] "График состояния очереди наладчика" "FixState" ++
      genQueueGraphics ["pq"] "График состояния очереди упаковщика" "PackState" ++
      genQueueGraphics ["tq"] "График состояния очереди транспортера" "TransportState" ++
      genQueueGraphics ["sq"] "График состояния очереди склада" "StoreState"
  }