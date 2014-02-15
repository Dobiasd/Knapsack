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

type Items = V.Vector Item

-- http://www.proggen.org/doku.php?id=algo:knapsack
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

items3 :: Items
items3 = V.fromList
        [
          Item 2 1
        , Item 3 20
        ]

items4 :: Items
items4 = V.fromList
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

{-
knapsack items wmax = arr ! wmax
  where
    arr = array (0, wmax) [(w, m w) | w <- [0..wmax]]
    m 0 = 0
    m w = maximum $ 0:[vi + arr ! (w - wi) | (vi, wi) <- items, wi <= w]
-}
solve2 :: Items -> Weight -> Value
solve2 items maxWeight = V.maximum firstMemoRow
    where
        memo = V.fromList [f idx weight | idx <- [0..(numItems-1)]
                                        , weight <- [0..maxWeight]]
        firstMemoRow = V.slice 0 (maxWeight+1) memo
        memoIndex idx weight = idx * (maxWeight+1) + weight
        numItems = V.length items
        getMemo idx weight
            | idx >= numItems = 0
            | otherwise = memo V.! (memoIndex idx weight)
        f idx weightLeft =
            if itemWeight > weightLeft
            then getMemo (idx + 1) weightLeft
            else max (getMemo (idx + 1) weightLeft)
                     (getMemo (idx + 1) (weightLeft - itemWeight)
                     + itemValue)
            where
                item = items V.! idx
                (itemValue, itemWeight) = (value item, weight item)


main :: IO ()
main = do
    print $ solve items1 M.empty (V.length items1 - 1) 30 -- -> 38
    print $ solve items2 M.empty (V.length items2 - 1) 15 -- -> 11
    print $ solve2 items1 30 -- -> 38
    print $ solve2 items2 15 -- -> 11
    print $ solve2 items3 10 -- -> 2