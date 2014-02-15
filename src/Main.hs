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

valueSum :: Result -> Value
valueSum (Result items) = map getValue items -: sum

newtype Result = Result [Item]

instance Ord Result where
    r1 `compare` r2 = (valueSum r1) `compare` (valueSum r2)

instance Eq Result where
    r1 == r2 = (valueSum r1) == (valueSum r2)

instance Show Result where
    show result@(Result items) =
        show (valueSum result) : map show items -: intercalate "\n"

type Solver = Items -> Index -> [Item] -> Weight -> Result

solveGoodIdx :: Solver -> Items -> Index -> [Item] -> Weight -> Result
solveGoodIdx go items idx inside weightLeft
    | itemWeight > weightLeft = go items (idx + 1) inside weightLeft
    | otherwise =
        max (go items (idx + 1) inside weightLeft)
            (go items (idx + 1) (inside ++ [item]) (weightLeft - itemWeight))
    where
        item = items V.! idx
        itemWeight = getWeight item

solveNaiveGo :: Items -> Index -> [Item] -> Weight -> Result
solveNaiveGo items idx inside weightLeft
    | idx >= numItems = Result inside
    | otherwise = solveGoodIdx solveNaiveGo items idx inside weightLeft
    where numItems = V.length items

solveNaive :: Items -> Weight -> Result
solveNaive items maxWeight = solveNaiveGo items 0 [] maxWeight

solveMemo :: Items -> Weight -> Result
solveMemo items maxWeight = Result $ backTrack maxIdx
    where
        memo = V.fromList [solveGoodIdx getMemo items i [] w |
                               i <- [0..(numItems-1)]
                             , w <- [0..maxWeight]]
        firstMemoRow = V.slice 0 (maxWeight+1) memo
        memoIndex idx weight = idx * (maxWeight+1) + weight
        numItems = V.length items
        getMemo :: Items -> Index -> [Item] -> Weight -> Result
        getMemo _ idx _ weight
            | idx >= numItems = Result []
            | otherwise = memo V.! (memoIndex idx weight)
        maxIdx = V.maxIndex firstMemoRow
        backTrack _ = []

args2Func :: [String] -> (Items -> Weight -> Result)
args2Func ("naive":_) = solveNaive
args2Func ("memo":_) = solveMemo
args2Func mode = error $ "unknown mode: " ++ show mode

main :: IO ()
main = do
    solve <- args2Func <$> getArgs
    print $ solve items1 30