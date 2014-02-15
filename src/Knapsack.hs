module Knapsack where

{-| todo bla -}

import Data.List
import Data.Function
import Text.Printf
import qualified Data.Map as M
import qualified Data.Vector as V

{-| forward application -}
(-:) :: a -> (a -> b) -> b
x -: f = f x
infixl 0 -:

type Value = Int
type Weight = Int
data Item = Item { value :: Value, weight :: Weight } deriving Show

-- http://www.proggen.org/doku.php?id=algo:knapsack
type Items = V.Vector Item
items1 :: Items
items1 = V.fromList
        [
          Item 10 30
        , Item  8  5
        , Item  8  5
        , Item  6  6
        , Item  5  8
        , Item 10 10
        , Item  5 11
        , Item 10 12
        , Item 17 15
        , Item 20 15
        , Item 20 30
        ]

items2 :: Items
items2 = V.fromList
        [
          Item 4 12
        , Item 2  1
        , Item 6  4
        , Item 1  1
        , Item 2  2
        ]

type Index = Int
type Params = (Index, Weight)
-- type Result = [Item]
type Memo = M.Map Params Value

solveFresh :: Items -> Memo -> Index -> Weight -> Value
solveFresh _ _ (-1) _ = 0
solveFresh items memo idx weightLeft =
    if itemWeight > weightLeft
    then solve items memo (idx - 1) weightLeft
    else max (solve items memo (idx - 1) weightLeft)
             (solve items memo (idx - 1) (weightLeft - itemWeight)
             + itemValue)
    where
        item = items V.! idx
        (itemValue, itemWeight) = (value item, weight item)


solve :: Items -> Memo -> Index -> Weight -> Value
solve items memo idx weightLeft =
    case M.lookup (idx, weightLeft) memo of
        (Just result) -> result
        Nothing -> solveFresh items memo idx weightLeft

main :: IO ()
main = do
    print $ solve items1 M.empty (V.length items1 - 1) 30 -- -> 38
    print $ solve items2 M.empty (V.length items2 - 1) 15 -- -> 11