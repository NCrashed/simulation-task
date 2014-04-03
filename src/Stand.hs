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

stand :: (Computer -> Dynamics Double) -> Simulation Stand
stand distr = do
  queue <- newFCFSQueue 100 
  workTimeRef <- newRef 0.0
  return $ Stand queue workTimeRef $ forever $ do
    comp <- dequeue queue
    repairTime <- liftDynamics $ distr comp
    holdProcess repairTime
    liftEvent $ modifyRef workTimeRef (+ repairTime)
    
standOne :: Simulation Stand
standOne =
  stand $ \comp -> case comp of
    ComputerType1 -> liftIO $ exprnd (1.0 / 7.0)
    ComputerType2 -> liftIO $ exprnd (1.0 / 10.0)

standTwo :: Simulation Stand
standTwo =
  stand $ \comp -> case comp of
    ComputerType1 ->  undefined
    ComputerType2 -> liftIO $ exprnd (1.0 / 6.0)