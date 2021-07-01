module Solution where

import Backpack ( Backpack(..), BackpackItem(weight, price) )
import Data.Vector
    ( (!),
      (++),
      drop,
      fromList,
      ifoldl,
      length,
      replicate,
      take,
      toList,
      Vector )
import Random ( getRandomNumber )

replace :: Vector a -> Int -> a -> Vector a
replace vector num target =
  Data.Vector.take num vector
    Data.Vector.++ fromList [target]
    Data.Vector.++ Data.Vector.drop (num + 1) vector

data Solution = Solution
  { nodes :: Vector Bool,
    result :: Int
  }

instance Show Solution where
  show solution = show (toList $ nodes solution) Prelude.++ show (result solution)

calculateSolution :: Backpack -> Vector Bool -> (Int, Int)
calculateSolution (Backpack items _) nodes =
  let availableIndices =
        ifoldl
          ( \prev index node ->
              if not node
                then prev
                else prev Prelude.++ [index]
          )
          []
          nodes
   in Prelude.foldl
        (\prev index -> (fst prev + price (items ! index), snd prev + weight (items ! index)))
        (0, 0)
        availableIndices

fillSolution :: Backpack -> IO Solution -> IO Solution
fillSolution backpack solution = do
  let maximumWeight = maxWeight backpack
  num <- getRandomNumber (0, toInteger (Data.Vector.length (items backpack) -1))
  currentNodes <- nodes <$> solution
  let newSolution = replace currentNodes num True
  let (newPrice, newWeight) = calculateSolution backpack newSolution
  if newWeight == maximumWeight
    then return $ Solution newSolution newPrice
    else
      if newWeight > maximumWeight
        then solution
        else fillSolution backpack (return $ Solution newSolution newPrice)

createSolution :: Backpack -> IO Solution
createSolution backpack =
  fillSolution
    backpack
    ( return $ Solution (Data.Vector.replicate (Data.Vector.length $ items backpack) False) 0
    )

getBestSolutionIndex :: Vector Solution -> Int
getBestSolutionIndex solutions =
  ifoldl
    ( \prev index curr ->
        if result (solutions ! prev) < result curr
          then index
          else prev
    )
    0
    solutions