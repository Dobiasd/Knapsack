module Main where

{-| bounded Knapsack problem -}

import Control.Applicative
import System.Environment
import qualified Data.Map as M
import qualified Data.Vector as V

{-| forward application -}
(-:) :: a -> (a -> b) -> b
x -: f = f x
infixl 0 -:

type Value = Int
type Weight = Int
data Item = Item { getValue :: Value, getWeight :: Weight } deriving Show

type Items = V.Vector Item

testItems :: Items
testItems = V.fromList
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
type Memo = M.Map Params Value
type Solver = Items -> Index -> Weight -> Value

solveGoodIdx :: Solver -> Items -> Index -> Weight -> Value
solveGoodIdx go items idx weightLeft
    | itemWeight > weightLeft = go items (idx + 1) weightLeft
    | otherwise =
        max (go items (idx + 1) weightLeft)
            (go items (idx + 1) (weightLeft - itemWeight) + itemValue)
    where
        item = items V.! idx
        (itemValue, itemWeight) = (getValue item, getWeight item)

solveNaiveGo :: Items -> Index -> Weight -> Value
solveNaiveGo items idx weightLeft
    | idx >= numItems = 0
    | otherwise = solveGoodIdx solveNaiveGo items idx weightLeft
    where numItems = V.length items

solveNaive :: Items -> Weight -> Value
solveNaive items maxWeight = solveNaiveGo items 0 maxWeight

solveMemo :: Items -> Weight -> Value
solveMemo items maxWeight = V.maximum firstMemoRow
    where
        memo = V.fromList [solveGoodIdx getMemo items i w |
                               i <- [0..(numItems-1)]
                             , w <- [0..maxWeight]]
        firstMemoRow = V.slice 0 (maxWeight+1) memo
        memoIndex idx weight = idx * (maxWeight+1) + weight
        numItems = V.length items
        getMemo _ idx weight
            | idx >= numItems = 0
            | otherwise = memo V.! (memoIndex idx weight)

args2Func :: [String] -> (Items -> Weight -> Value)
args2Func ("naive":_) = solveNaive
args2Func ("memo":_) = solveMemo
args2Func mode = error $ "unknown mode: " ++ show mode

main :: IO ()
main = do
    solve <- args2Func <$> getArgs
    print $ solve testItems 263 -- -> 889