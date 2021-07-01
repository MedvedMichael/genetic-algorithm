module GeneticAlgorithm where

import Backpack ( Backpack(maxWeight) )
import Crossingover ( crossingover )
import Data.Bifunctor (bimap)
import Data.Vector (Vector, drop, fromList, ifoldl, replicate, take, toList, (!))
import Mutation ( mutate )
import Random (getRandomNumber)
import Selection ( selection )
import Solution
    ( Solution(result, nodes), replace, calculateSolution )

data GeneticAlgorithmFactory = GeneticAlgorithmFactory
  { backpack :: Backpack,
    crossingoverNumbers :: [Int],
    solutions :: Vector Solution,
    bestSolutionIndex :: Int
  }

geneticAlgorithm :: IO GeneticAlgorithmFactory -> IO GeneticAlgorithmFactory
geneticAlgorithm fact =
  do
    factory <- fact
    selected <- selection (solutions factory) (bestSolutionIndex factory)
    let (firstChild, _) = crossingover selected (crossingoverNumbers factory)
    mutatedChild <- mutate firstChild
    -- print "---Start---"
    -- print firstChild
    -- print mutatedChild
    -- print "---End---"
    let (newPrice, newWeight) = calculateSolution (backpack factory) (nodes mutatedChild)
    if newWeight > maxWeight (backpack factory)
      then return factory
      else
        let oldSolutions = solutions factory
            lastBestPrice = result $ oldSolutions ! bestSolutionIndex factory
         in if lastBestPrice > newPrice
              then return factory
              else
                let worstIndex =
                      ifoldl
                        ( \prev index curr ->
                            if result (oldSolutions ! prev) > result curr
                              then prev
                              else index
                        )
                        0
                        oldSolutions
                    newSolutions =
                      replace
                        oldSolutions
                        worstIndex
                        firstChild
                          { result = newPrice
                          }
                 in return
                      factory
                        { solutions = newSolutions,
                          bestSolutionIndex = worstIndex
                        }
