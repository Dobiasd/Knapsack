module Main where

{-| 0-1 knapsack problem -}

import Control.Applicative
import Data.Function
import Data.List
import System.Environment
import System.Random
import Text.Printf
import qualified Data.Map as M
import qualified Data.Vector as V

{-| forward application -}
(-:) :: a -> (a -> b) -> b
x -: f = f x
infixl 0 -:

type Value = Int
type Weight = Int
data Item = Item { getName :: String
                 , getValue :: Value
                 , getWeight :: Weight } deriving (Ord, Eq)
type Items = V.Vector Item

randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

testItems :: Items
testItems = V.fromList $ zipWith3 Item names randomValues randomWeights
    where
        names = map show ([0..] :: [Int])
        randomValues =  randomList 100 (mkStdGen 0) -: map clamp
        randomWeights = randomList 100 (mkStdGen 1) -: map clamp
        clamp = (+1) . (`mod` 20) -- strictly positive integers

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

getWeightSum :: Result -> Weight
getWeightSum = sum . map getWeight . getItems

instance Ord Result where
    compare = compare `on` getValueSum

instance Eq Result where
    (==) = (==) `on` getValueSum

checkResult :: Result -> Bool
checkResult (Result valueSum items) = valueSum == (map getValue items -: sum)

instance Show Item where
    show (Item name value weight) =
        [ printf "name: %2s" name
        , printf "value: %2d" value
        , printf "weight: %2d" weight ] -: intercalate "   "

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

type MemoGetter = Index -> Weight -> Value
type BackTracking = Items -> MemoGetter -> Weight -> Result
backTrack :: BackTracking
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

backTrackProggenOrd :: BackTracking
backTrackProggenOrd items getMemo maxWeight =
    Result valueSum (map (items V.!) (reverse indices))
    where
        valueSum = getMemo 0 bestWeight
        numItems = V.length items
        bestWeight = maxIndex $ map (getMemo 0) [0..maxWeight]

        indices :: [Index]
        indices = until (\(i, w, _) -> i >= numItems || w <= 0)
                        nextPos (0, bestWeight, []) -: (\(_, _, l) -> l)
        nextPos (i, w, l)
            | possNextWeight >= 0 &&
              currValue - itemValue == possNextValue =
                (i + 1, w - itemWeight, i:l)
            | otherwise = (i + 1, w, l)
            where
                possNextWeight = w - itemWeight
                possNextValue = getMemo (i + 1) possNextWeight
                currValue = getMemo i w
                itemWeight = getWeight item
                itemValue = getValue item
                item = items V.! i

solveMemo :: BackTracking -> Items -> Weight -> Result
solveMemo backtracking items maxWeight =
    backtracking items getMemo maxWeight
    --error $ show (chunksOf (maxWeight+1) (V.toList memo))
    --error $ show items
    where
        memo :: V.Vector Value
        memo = V.fromList [ getValueSum $ solveGoodIdx memoSolver items i w |
                               i <- [0..(numItems-1)]
                             , w <- [0..maxWeight] ]
        memoIndex idx weight = idx * (maxWeight+1) + weight
        getMemo :: MemoGetter
        getMemo idx weight
            | idx < 0 = error "idx < 0"
            | weight < 0 = error "weight < 0"
            | weight > maxWeight = error "weight > maxWeight"
            | idx >= numItems = 0
            | otherwise = val
            where val = memo V.! memoIndex idx weight
        memoSolver _ idx weight = Result (getMemo idx weight) []
        numItems = V.length items

args2Func :: [String] -> Items -> Weight -> Result
args2Func ("naive":_) = solveNaive
args2Func ("memo":_) = solveMemo backTrack
args2Func mode = error $ "unknown mode: " ++ show mode

test :: Index -> Index -> Weight -> Bool
test firstItem numItems maxWeight =
    getValueSum memoResult == getValueSum naiveResult &&
    getValueSum memoPOResult == getValueSum naiveResult
    where
        memoResult = solveMemo backTrack items maxWeight
        memoPOResult = solveMemo backTrackProggenOrd items maxWeight
        naiveResult = solveNaive items maxWeight
        items
            | numItems > V.length testItems = error "Invalid item count."
            | otherwise = V.slice firstItem numItems testItems

tests :: IO ()
tests = do
    putStrLn "Running tests ..."
    let results = [((firstItem, numItems, maxWeight), test firstItem numItems maxWeight) |
                  firstItem <- [0..20], numItems <- [0..20], maxWeight <- [0..50]]
    let badResults = filter (not . snd) results
    putStrLn $ if null badResults then "Tests OK."
                                  else "Tests failed:\n" ++ show badResults

main :: IO ()
main = do
    solve <- args2Func <$> getArgs
    tests
    --print $ solve (V.slice 0 40 testItems) 34
    --print $ solve (V.slice 0 6 testItems) 22
    print $ solve items1 30