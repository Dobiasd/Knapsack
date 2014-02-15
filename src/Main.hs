module Main where

{-| bounded Knapsack problem -}

import Control.Applicative
import Data.List
import System.Environment
import System.Random
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


randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

testItems :: Items
testItems = V.fromList $ zipWith Item randomValues randomWeights
    where
        randomValues =  randomList 100 (mkStdGen 0) -: map (`mod` 100)
        randomWeights = randomList 100 (mkStdGen 1) -: map (`mod` 100)

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
    print $ solve testItems 4000 -- -> 889