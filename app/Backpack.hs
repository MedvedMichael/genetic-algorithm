module Backpack where

import Data.Vector (Vector)
import Prelude (Int, Show, show, (++))

data BackpackItem = BackpackItem
  { price :: Int,
    weight :: Int
  }

instance Show BackpackItem where
  show item = "Price: " ++ show (price item) ++ ", weight: " ++ show (weight item)

data Backpack = Backpack
  { items :: Vector BackpackItem,
    maxWeight :: Int
  }