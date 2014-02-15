module Main where

{-| todo bla -}

import Control.Applicative
import Data.Function
import Data.List
import Text.Printf
import System.Environment
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
          Item 28  52
        , Item 3   25
        , Item 29  52
        , Item 86  51
        , Item 17  80
        , Item 30  71
        , Item 6   25
        , Item 8   46
        , Item 42  97
        , Item 65  82
        , Item 94  97
        , Item 90  36
        , Item 37  22
        , Item 44  76
        , Item 48  95
        , Item 92  9
        , Item 83  50
        , Item 57  4
        , Item 36  47
        , Item 3   17
        , Item 83  73
        , Item 72  77
        , Item 37  7
        , Item 95  54
        , Item 54  51
        , Item 93  98
        , Item 35  51
        , Item 33  93
        , Item 47  30
        , Item 13  48
        , Item 28  38
        , Item 99  19
        , Item 62  96
        , Item 2   31
        , Item 62  28
        , Item 7   39
        , Item 60  80
        , Item 5   91
        , Item 65  9
        , Item 56  96
        , Item 73  95
        , Item 95  92
        , Item 55  69
        , Item 47  1
        , Item 35  98
        , Item 29  59
        , Item 76  20
        , Item 35  23
        , Item 15  6
        , Item 46  2
        ]

type Index = Int
type Params = (Index, Weight)
-- type Result = [Item]
type Memo = M.Map Params Value

solveNaiveGo :: Items -> Index -> Weight -> Value
solveNaiveGo _ (-1) _ = 0
solveNaiveGo items idx weightLeft =
    if itemWeight > weightLeft
    then solveNaiveGo items (idx - 1) weightLeft
    else max (solveNaiveGo items (idx - 1) weightLeft)
             (solveNaiveGo items (idx - 1) (weightLeft - itemWeight)
             + itemValue)
    where
        item = items V.! idx
        (itemValue, itemWeight) = (value item, weight item)

solveNaive :: Items -> Weight -> Value
solveNaive items maxWeight = solveNaiveGo items (V.length items - 1) maxWeight

solveMemo :: Items -> Weight -> Value
solveMemo items maxWeight = V.maximum firstMemoRow
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

args2Func ("naive":_) = solveNaive
args2Func ("memo":_) = solveMemo
args2Func mode = error $ "unknown mode: " ++ show mode

main :: IO ()
main = do
    solve <- args2Func <$> getArgs

    print $ solve items1 30 -- -> 38
    print $ solve items2 15 -- -> 11
    print $ solve items3 10 -- -> 2
    print $ solve items4 263 -- -> 889