module Main where

{-| bounded Knapsack problem -}

import Control.Applicative
import Data.Function
import Data.List
import Data.List.Split
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
        names = map show ([0..] :: [Int])
        randomValues =  randomList 100 (mkStdGen 0) -: map clamp
        randomWeights = randomList 100 (mkStdGen 1) -: map clamp
        clamp = (+1) . (`mod` 90)

-- weight limit 30 -> (38, ["0", "4", "8"])
items1 :: Items
items1 = V.fromList
        [
          Item "0"  8  5
        , Item "1"  8  5
        , Item "2"  6  6
        , Item "3"  5  8
        , Item "4" 10 10
        , Item "5"  5 11
        , Item "6" 10 12
        , Item "7" 17 15
        , Item "8" 20 15
        , Item "9" 20 30
        ]

type Index = Int
type Params = (Index, Weight)
type Memo = M.Map Params Value

data Result = Result { getValueSum :: Value, getItems :: [Item] }

instance Ord Result where
    compare = compare `on` getValueSum

instance Eq Result where
    (==) = (==) `on` getValueSum

checkResult :: Result -> Bool
checkResult (Result valueSum items) = True -- valueSum == (map getValue items -: sum)

instance Show Result where
    show result@(Result valueSum items)
        | checkResult result = show valueSum
                               : show calculatedSum
                               : map show items -: intercalate "\n"
        | otherwise = error $ show calculatedSum ++ "/=" ++ show valueSum
        where calculatedSum = map getValue items -: sum

addItem :: Result -> Item -> Result
(Result valSum items) `addItem` item@(Item _ val _) =
    Result (valSum + val) (item:items)

type Solver = Items -> Index -> Weight -> Result

solveGoodIdx :: Solver -> Solver
solveGoodIdx go items idx weightLeft
    | itemWeight > weightLeft = go items (idx + 1) weightLeft
    | otherwise =
        max (go items (idx + 1) weightLeft)
            (go items (idx + 1) (weightLeft - itemWeight) `addItem` item)
    where
        item = items V.! idx
        itemWeight = getWeight item

solveNaiveGo :: Solver
solveNaiveGo items idx weightLeft
    | idx >= numItems = Result 0 []
    | otherwise = solveGoodIdx solveNaiveGo items idx weightLeft
    where numItems = V.length items

solveNaive :: Items -> Weight -> Result
solveNaive items = solveNaiveGo items 0

maxIndex :: Ord a => [a] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

{-
value in cells
x = -1
weight     012345
index
        0      9x
        1
        2
        3
-}
backTrack :: Items -> (Index -> Weight -> Value) -> Weight -> Result
backTrack items getMemo maxWeight =
    Result valueSum (map (items V.!) (reverse indices))
    where
        valueSum = getMemo 0 bestWeight
        numItems = V.length items
        bestWeight = maxIndex $ map (getMemo 0) [0..maxWeight]

        indices :: [Index]
        indices = until (\(i, w, _) -> i >= numItems || w <= 0)
                        nextPos (0, bestWeight, []) -: (\(_, _, l) -> l)
        nextPos (i, w, l)
            | getMemo (i + 1) w == getMemo i w = (i + 1, w, l)
            | otherwise = (i + 1, w - getWeight item, i:l)
            where
                item = items V.! i

solveMemo :: Items -> Weight -> Result
solveMemo items maxWeight = backTrack items getMemo maxWeight
    --error $ show (chunksOf (maxWeight+1) (V.toList memo))
    --error $ show items
    where
        memo :: V.Vector Value
        memo = V.fromList [ getValueSum $ solveGoodIdx memoSolver items i w |
                               i <- [0..(numItems-1)]
                             , w <- [0..maxWeight] ]
        memoIndex idx weight = idx * (maxWeight+1) + weight
        getMemo :: Index -> Weight -> Value
        getMemo idx weight
            | idx >= numItems = 0
            | otherwise = val
            where val = memo V.! memoIndex idx weight
        memoSolver _ idx weight = Result (getMemo idx weight) []
        numItems = V.length items


args2Func :: [String] -> Items -> Weight -> Result
args2Func ("naive":_) = solveNaive
args2Func ("memo":_) = solveMemo
args2Func mode = error $ "unknown mode: " ++ show mode


main :: IO ()
main = do
    solve <- args2Func <$> getArgs
    print $ solve testItems 60
    print $ solve items1 30