{-# LANGUAGE DoAndIfThenElse #-}
module Packer(
    Packer(..)
  , Palete(..)
  , packer
  ) where

import Computer
import Util
import Control.Monad.Trans

import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Queue
import Simulation.Aivika.Ref
import Control.Monad (forever, when)
import Simulation.Aivika.Event (liftEvent)
     
data Packer = Packer
  -- | Очередь  
  (FCFSQueue Computer) 
  -- | Общее время работы
  (Ref Double) 
  -- | Количество компьютеров в палете
  (Ref Int)
  -- | Обработчик
  (Process ()) 

-- | Палета
newtype Palete = Palete Int

packer :: FCFSQueue Palete -> Int -> Simulation Packer
packer transportQueue paleteMaxSize = do
  queue <- newFCFSQueue 200 
  workTimeRef   <- newRef 0.0
  paleteSizeRef <- newRef 0
  return $ Packer queue workTimeRef paleteSizeRef $ forever $ do
    _ <- dequeue queue
    packTime <- liftIO $ uniform' 1 0.5
    holdProcess packTime
    liftEvent $ modifyRef workTimeRef (+ packTime)
    liftEvent $ modifyRef paleteSizeRef (+ 1)
    paleteSize <- liftEvent $ readRef paleteSizeRef
    when (paleteSize >= paleteMaxSize) $ do
      enqueue transportQueue $ Palete paleteMaxSize
      liftEvent $ writeRef paleteSizeRef 0