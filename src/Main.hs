module Main where

{-| bounded Knapsack problem -}

import Control.Applicative
import Data.Function
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
data Item = Item { name :: String
                 , getValue :: Value
                 , getWeight :: Weight } deriving (Show, Ord, Eq)
type Items = V.Vector Item

randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

testItems :: Items
testItems = V.fromList $ zipWith3 Item names randomValues randomWeights
    where
        names = map show ([1..] :: [Int])
        randomValues =  randomList 100 (mkStdGen 0) -: map clamp
        randomWeights = randomList 100 (mkStdGen 1) -: map clamp
        clamp = (+1) . (`mod` 90)

-- weight limit 30 -> (38, ["1", "5", "9"])
items1 :: Items
items1 = V.fromList
        [
          Item  "1"  8  5
        , Item  "2"  8  5
        , Item  "3"  6  6
        , Item  "4"  5  8
        , Item  "5" 10 10
        , Item  "6"  5 11
        , Item  "7" 10 12
        , Item  "8" 17 15
        , Item  "9" 20 15
        , Item "10" 20 30
        ]

type Index = Int
type Params = (Index, Weight)
type Memo = M.Map Params Value

-- Naive solution fills both fields.
--  It would only need the second, since the first
--  can be calculated on demand, but this would again
--  lift its algorithmic complexity.
-- The memoized solution only uses the first field,
--  since it can not know about the second.
--  The items are backtracked after everything is done.
data Result = Result { getValueSum :: Int, getItems :: [Item] }

instance Ord Result where
    compare = compare `on` getValueSum

instance Eq Result where
    (==) = (==) `on` getValueSum

instance Show Result where
    show (Result valueSum items) =
        show valueSum : map show items -: intercalate "\n"

add :: Result -> Result -> Result
(Result v1 l1) `add` (Result v2 l2) = Result (v1 + v2) (l1 ++ l2)

type Solver = Items -> Index -> [Item] -> Weight -> Result

solveGoodIdx :: Solver -> Items -> Index -> [Item] -> Weight -> Result
solveGoodIdx go items idx inside weightLeft
    | itemWeight > weightLeft = go items (idx + 1) inside weightLeft
    | otherwise =
        max (go items (idx + 1) inside weightLeft)
            (go items (idx + 1) (inside ++ [item]) (weightLeft - itemWeight)
                `add` Result itemValue [])
    where
        item = items V.! idx
        (itemValue, itemWeight) = (getValue item, getWeight item)

solveNaiveGo :: Items -> Index -> [Item] -> Weight -> Result
solveNaiveGo items idx inside weightLeft
    | idx >= numItems = Result 0 inside
    | otherwise = solveGoodIdx solveNaiveGo items idx inside weightLeft
    where numItems = V.length items

solveNaive :: Items -> Weight -> Result
solveNaive items = solveNaiveGo items 0 []

maxIndex :: Ord a => [a] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

backTrack :: Items -> (Index -> Weight -> Result) -> Weight -> Result
backTrack items getMemo maxWeight =
    Result valueSum (map (items V.!) (f [] 0 valueSum))
    where
        valueSum = getValueSum $ getMemo 0 maxWeightIdx
        numItems = V.length items
        maxWeightIdx = maxIndex $ map (getMemo 0) [0..maxWeight]
        f :: [Index] -> Index -> Weight -> [Index]
        f acc idx weightSum
            | idx >= numItems = acc
            | nextValSum == valSum - getValue item =
                f (acc ++ [idx]) (idx + 1) nextWeightSum
            | otherwise = f acc (idx + 1) weightSum
            where
                valSum = getValueSum $ getMemo idx weightSum
                nextWeightSum = weightSum - itemWeight
                nextValSum
                    | nextWeightSum >= 0 =
                        getValueSum $ getMemo (idx + 1) nextWeightSum
                    | otherwise = -1
                item = items V.! idx
                itemWeight = getWeight item


solveMemo :: Items -> Weight -> Result
solveMemo items maxWeight = backTrack items getMemo maxWeight
    where
        memo = V.fromList [solveGoodIdx memoSolver items i [] w |
                               i <- [0..(numItems-1)]
                             , w <- [0..maxWeight]]
        memoIndex idx weight = idx * (maxWeight+1) + weight
        getMemo idx weight
            | idx >= numItems = Result 0 []
            | otherwise = memo V.! memoIndex idx weight
        memoSolver _ idx _ weight = getMemo idx weight
        numItems = V.length items


args2Func :: [String] -> Items -> Weight -> Result
args2Func ("naive":_) = solveNaive
args2Func ("memo":_) = solveMemo
args2Func mode = error $ "unknown mode: " ++ show mode

main :: IO ()
main = do
    solve <- args2Func <$> getArgs
    print $ solve items1 30
    --print $ solve (V.fromList $ [Item "1" 10 10, Item "2" 2 2]) 5