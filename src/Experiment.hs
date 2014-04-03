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
      , outputView $ defaultTimeSeriesView 
        { timeSeriesTitle = "Stand queues sizes"
        , timeSeries = [Left "q1", Left "q2"]
        }
      ]
  }