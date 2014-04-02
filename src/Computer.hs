module Computer(
    exprnd
  , Computer(..)
  , computerStream
  ) where
  
import System.Random
import Control.Monad.Trans

import Simulation.Aivika.Process
import Simulation.Aivika.Event
import Simulation.Aivika.Queue

exprnd :: Double -> IO Double
exprnd lambda = do
  x <- getStdRandom random
  return (- log x / lambda)
  
data Computer = ComputerType1 | ComputerType2

generateComputer :: IO Computer
generateComputer = do
  x <- getStdRandom random :: IO Double
  return (if x <= 0.7 then ComputerType1 else ComputerType2)

computerStream :: FCFSQueue Computer -> FCFSQueue Computer -> Process ()
computerStream qstand1 qstand2 = do
  genTime <- liftIO $ exprnd (1.0 / 0.5)
  holdProcess genTime
  comp <- liftIO generateComputer 
  isStand2Bysy <- liftEvent $ queueNull qstand2 
  case comp of
    ComputerType1 -> enqueue qstand1 comp
    ComputerType2 -> if isStand2Bysy
      then enqueue qstand2 comp
      else enqueue qstand1 comp