module Stand(
    Stand(..)
  , standOne
  , standTwo
  , runStand
  ) where

import Computer
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Simulation
import Simulation.Aivika.Process
import Simulation.Aivika.Queue
     
data Stand = Stand (FCFSQueue Computer) (Computer -> Dynamics Double) (Process ()) 

stand :: (Computer -> Dynamics Double) -> Simulation Stand
stand distr = do
  queue <- newFCFSQueue 100 
  return $ Stand queue distr $ return ()

standOne :: Simulation Stand
standOne =
  stand $ \comp -> case comp of
    ComputerType1 -> liftIO $ exprnd (7*60)
    ComputerType2 -> liftIO $ exprnd (10*60)

standTwo :: Simulation Stand
standTwo =
  stand $ \comp -> case comp of
    ComputerType1 ->  undefined
    ComputerType2 -> liftIO $ exprnd (6*60)
     
runStand :: Stand -> Simulation ()
runStand (Stand _ _ func) = do
  pid <- newProcessId
  runProcessInStartTimeUsingId pid func 