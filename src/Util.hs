module Util(
    stdDistr
  , exprnd
  ) where
  
import System.Random

stdDistr :: IO Double
stdDistr = getStdRandom $ randomR (0.0, 1.0)

exprnd :: Double -> IO Double
exprnd lambda = do
  x <- stdDistr
  return (- log x / lambda)
  
  