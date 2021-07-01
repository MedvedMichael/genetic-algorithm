module Crossingover where

import Data.Vector (drop, take, (++))
import Solution ( Solution(Solution, nodes) )
import Prelude (Int, fst, null, snd, ($), IO, return, print, map, (-))

crossingover :: (Solution, Solution) -> [Int] -> (Solution, Solution)
crossingover solutions indices =
  let (Solution firstParentNodes _, Solution secondParentNodes _) = solutions
      (crossIndex : as) = indices
   in if null indices
        then solutions
        else
          let firstChild =
                take crossIndex firstParentNodes
              secondChild =
                take crossIndex secondParentNodes
              newSolutions =
                crossingover
                  ( Solution (drop crossIndex secondParentNodes) 0,
                    Solution (drop crossIndex firstParentNodes) 0
                  )
                  (map (\a -> a - crossIndex) as)
          in (Solution (firstChild ++ nodes (fst newSolutions)) 0, Solution (secondChild ++ nodes (snd newSolutions)) 0)
