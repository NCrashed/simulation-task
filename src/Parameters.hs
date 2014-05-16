module Parameters where
  
computer1Rate :: Double
computer1Rate = 0.7

computer1Stand1Time :: Double
computer1Stand1Time = 7.0

computer2Stand1Time :: Double
computer2Stand1Time = 10.0

computer1Stand2Time :: Double
computer1Stand2Time = undefined

computer2Stand2Time :: Double
computer2Stand2Time = 6.0

computerGeneratorIntense :: Double
computerGeneratorIntense = 0.5

controllerTimeDistr :: (Double, Double)
controllerTimeDistr = (6, 12)

controllerSuccessRate :: Double
controllerSuccessRate = 0.85

fixTimeDist :: (Double, Double)
fixTimeDist = (20, 40)

paleteMaxSize :: Int
paleteMaxSize = 12

packTimeDistr :: (Double, Double)
packTimeDistr = (1, 0.5)

transportTimeDistr :: (Double, Double)
transportTimeDistr = (16, 4)