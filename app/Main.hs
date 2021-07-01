module Main where

import Backpack ( Backpack(Backpack), BackpackItem(BackpackItem) )
import Data.List (findIndex, intercalate)
import Data.Vector (fromList, generateM, ifoldl, imap, map, replicateM, toArray, toList, (!))
import GeneticAlgorithm
    ( GeneticAlgorithmFactory(GeneticAlgorithmFactory),
      geneticAlgorithm )
import Solution ( Solution(result), createSolution )

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn arr delimiters =
  case findIndex (`elem` delimiters) arr of
    Just num -> take num arr : splitOn (drop (num + 1) arr) delimiters
    Nothing -> [arr]

getBackpackFromFile :: String -> IO Backpack
getBackpackFromFile path = do
  file <- readFile path
  let lines = splitOn file "\n"
  let (maxWeightStr : pairs) = lines
  let backpack =
        Prelude.map
          ( (\pair -> BackpackItem (head pair) (last pair))
              . ( \pairStr ->
                    Prelude.map
                      (\a -> read a :: Int)
                      (splitOn pairStr " ")
                )
          )
          pairs

  return (Backpack (fromList backpack) (read maxWeightStr :: Int))

main :: IO ()
main = do
  startBackpack <- getBackpackFromFile "files/backpack.txt"
  startSolutions <- generateM 10 (\a -> createSolution startBackpack)
  let bestIndex = 0
  let geneticFactory = GeneticAlgorithmFactory startBackpack [3] startSolutions bestIndex
  let iteratedFactory = iterate geneticAlgorithm (return geneticFactory)
  -- curr <- iteratedFactory !! 3
  -- let curr =
  print $ startSolutions ! bestIndex
  (GeneticAlgorithmFactory _ _ newSolutions _) <- iteratedFactory !! 10000
  let bestCurrSolution =
        newSolutions
          ! ifoldl
            ( \prev index a ->
                if result a > result (newSolutions ! prev)
                  then index
                  else prev
            )
            0
            newSolutions
  print bestCurrSolution
