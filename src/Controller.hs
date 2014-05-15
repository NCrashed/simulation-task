module Controller(
    Controller(..)
  , controller
  ) where

import Computer
import Util
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Queue
import Simulation.Aivika.Ref
import Control.Monad (forever)
import Simulation.Aivika.Event (liftEvent)
import Debug.Trace (trace)
     
data Controller = Controller
  -- | Общее время работы
  (Ref Double) 
  -- | Обработчик стенда
  (Process ()) 

controller :: FCFSQueue Computer -> FCFSQueue Computer -> FCFSQueue Computer -> Simulation Controller
controller fixQueue packQueue stationQueue = do
  workTimeRef <- newRef 0.0
  return $ Controller workTimeRef $ forever $ do
    comp <- dequeue stationQueue
    checkTime <- liftIO $ uniform 6 12
    holdProcess checkTime
    randomChoice 0.85
      (trace "pushed to pack!" $ enqueue packQueue comp)
      (enqueue fixQueue comp) 
    liftEvent $ modifyRef workTimeRef (+ checkTime)