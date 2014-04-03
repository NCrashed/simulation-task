module Experiment(
    specs
  , experiment
  ) where
  
import Simulation.Aivika.Specs
import Simulation.Aivika.Generator
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.TimeSeriesView

specs :: Specs
specs = Specs { spcStartTime = 0.0, 
                spcStopTime = 100.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
                
experiment :: Experiment
experiment = 
  defaultExperiment 
  { experimentSpecs = specs
  , experimentRunCount = 1
  , experimentTitle = "Домашнее задание по курсу 'Иммитационное моделирование'"
  , experimentDescription = ""
  , experimentGenerators = 
      [ outputView defaultExperimentSpecsView
      , outputView $ defaultFinalStatsView 
        { finalStatsTitle = "Статистика очереди Стенда1"
        , finalStatsSeries = ["q1_wait"] 
        }
      , outputView $ defaultFinalStatsView 
        { finalStatsTitle = "Статистика очереди Стенда2"
        , finalStatsSeries = ["q2_wait"] 
        }
      , outputView $ defaultLastValueView
        { lastValueTitle = "Характеристики очереди Стенда1"
        , lastValueSeries = ["q1_extracted", "q1_rate", "q1_load"]
        }
      , outputView $ defaultLastValueView
        { lastValueTitle = "Характеристики очереди Стенда2"
        , lastValueSeries = ["q2_extracted", "q2_rate", "q2_load"]
        }
      , outputView $ defaultTimeSeriesView 
        { timeSeriesTitle = "График состояния очередей стендов"
        , timeSeriesFileName = UniqueFileName "StandState - $RUN_INDEX" ".png"
        , timeSeries = [Left "q1", Left "q2"]
        }
      ]
  }