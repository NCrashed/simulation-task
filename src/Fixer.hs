module Fixer(
    Fixer(..)
  , fixer
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
     
data Fixer = Fixer
  -- | Очередь  
  (FCFSQueue Computer) 
  -- | Общее время работы
  (Ref Double) 
  -- | Обработчик
  (Process ()) 

fixer :: FCFSQueue Computer -> Simulation Fixer
fixer packQueue = do
  queue <- newFCFSQueue 200 
  workTimeRef <- newRef 0.0
  return $ Fixer queue workTimeRef $ forever $ do
    comp <- dequeue queue
    fixTime <- liftIO $ uniform 20 40
    holdProcess fixTime
    enqueue packQueue comp
    liftEvent $ modifyRef workTimeRef (+ fixTime)