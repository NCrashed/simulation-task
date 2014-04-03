module Computer(
    exprnd
  , Computer(..)
  , computerStream
  ) where
  
import Util  

import Simulation.Aivika.Process
import Simulation.Aivika.Event
import Simulation.Aivika.Queue
import Control.Monad.Trans

data Computer = ComputerType1 | ComputerType2

generateComputer :: IO Computer
generateComputer = do
  x <- stdDistr
  return (if x <= 0.7 then ComputerType1 else ComputerType2)

computerStream :: FCFSQueue Computer -> FCFSQueue Computer -> Process ()
computerStream qstand1 qstand2 = do
  genTime <- liftIO $ exprnd 0.5
  holdProcess genTime
  comp <- liftIO generateComputer 
  isStand2Free <- liftEvent $ queueNull qstand2 
  case comp of
    ComputerType1 -> enqueue qstand1 comp
    ComputerType2 -> if isStand2Free
      then enqueue qstand2 comp
      else enqueue qstand1 comp
  computerStream qstand1 qstand2