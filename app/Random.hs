module Random where

import Control.Monad (replicateM)
import System.Random (Random (random, randomR), getStdGen, getStdRandom, randomRIO)

getRandomNumber :: (Integer, Integer) -> IO Int
getRandomNumber bounds = do
  num <- randomRIO bounds :: IO Integer
  return (fromInteger num :: Int)

randomList :: Int -> (Integer, Integer) -> IO [Int]
randomList n bounds = replicateM n $ getRandomNumber bounds

randomBoolean :: IO Bool 
randomBoolean = do
  num <- randomRIO (0,1) :: IO Float
  return $ num < 0.5



randomBooleanList :: Int  -> IO [Bool]
randomBooleanList num = replicateM num randomBoolean
    
