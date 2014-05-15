module Stand(
    Stand(..)
  , standOne
  , standTwo
  ) where

import Computer
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Queue
import Simulation.Aivika.Ref
import Control.Monad (forever)
import Simulation.Aivika.Event (liftEvent)
     
data Stand = Stand
  -- | Очередь стенда 
  (FCFSQueue Computer) 
  -- | Общее время работы
  (Ref Double) 
  -- | Обработчик стенда
  (Process ()) 

stand :: FCFSQueue Computer -> (Computer -> Dynamics Double) -> Simulation Stand
stand station distr = do
  queue <- newFCFSQueue 100 
  workTimeRef <- newRef 0.0
  return $ Stand queue workTimeRef $ forever $ do
    comp <- dequeue queue
    repairTime <- liftDynamics $ distr comp
    holdProcess repairTime
    enqueue station comp
    liftEvent $ modifyRef workTimeRef (+ repairTime)
    
standOne :: FCFSQueue Computer -> Simulation Stand
standOne station =
  stand station $ \comp -> case comp of
    ComputerType1 -> liftIO $ exprnd (1.0 / 7.0)
    ComputerType2 -> liftIO $ exprnd (1.0 / 10.0)

standTwo :: FCFSQueue Computer -> Simulation Stand
standTwo station =
  stand station $ \comp -> case comp of
    ComputerType1 ->  undefined
    ComputerType2 -> liftIO $ exprnd (1.0 / 6.0)