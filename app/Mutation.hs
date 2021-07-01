module Mutation where

import Data.Vector ( (!), length )
import Random ( getRandomNumber, randomBoolean )
import Solution ( Solution(nodes), replace )

mutate :: Solution -> IO Solution
mutate solution = do
  check <- randomBoolean
  if not check
    then return solution
    else do
      index <- getRandomNumber (0, toInteger (Data.Vector.length $ nodes solution) -1)
      return
        solution
          { nodes = replace (nodes solution) index (not (nodes solution ! index))
          }