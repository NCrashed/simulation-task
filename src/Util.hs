module Util(
    stdDistr
  , exprnd
  , uniform
  , randomChoice
  ) where
  
import System.Random
import Control.Monad.Trans

stdDistr :: IO Double
stdDistr = getStdRandom $ randomR (0.0, 1.0)

exprnd :: Double -> IO Double
exprnd lambda = do
  x <- stdDistr
  return (- log x / lambda)
  
uniform :: Double -> Double -> IO Double
uniform a b 
  | a > b  = error $ "Invalid bounds [" ++ show a ++ "," ++ show b ++ "]"
  | a == b = return a
  | otherwise = do
    x <- stdDistr
    return $ x * (b - a) + a
    
randomChoice :: MonadIO m => Double -> m a -> m a -> m a
randomChoice chance thenAction elseAction = do
  x <- liftIO stdDistr
  if x > chance then thenAction else elseAction
  