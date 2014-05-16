module Transporter(
    Transporter(..)
  , transporter
  ) where

import Packer
import Util
import Parameters
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Queue
import Simulation.Aivika.Ref
import Control.Monad (forever)
import Simulation.Aivika.Event (liftEvent)
     
data Transporter = Transporter
  -- | Очередь  
  (FCFSQueue Palete) 
  -- | Общее время работы
  (Ref Double) 
  -- | Обработчик
  (Process ()) 

transporter :: FCFSQueue Palete -> Simulation Transporter
transporter storeQueue = do
  queue <- newFCFSQueue 200 
  workTimeRef <- newRef 0.0
  return $ Transporter queue workTimeRef $ forever $ do
    palete <- dequeue queue
    transportTime <- liftIO $ uncurry uniform' transportTimeDistr
    holdProcess transportTime
    enqueue storeQueue palete
    liftEvent $ modifyRef workTimeRef (+ transportTime)