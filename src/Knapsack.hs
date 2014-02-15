module Knapsack where

{-| todo bla -}

import qualified Data.Set as Set
import Data.List
import Data.Function
import Text.Printf

{-| forward application -}
(-:) :: a -> (a -> b) -> b
x -: f = f x
infixl 0 -:

type Volume = Int
type Value = Int
data Item = Item { volume :: Volume, value :: Value } deriving Show

-- http://www.proggen.org/doku.php?id=algo:knapsack
items :: [Item]
items = [
          Item 30 10
        , Item  5  8
        , Item  5  8
        , Item  6  6
        , Item  8  5
        , Item 10 10
        , Item 11  5
        , Item 12 10
        , Item 15 17
        , Item 15 20
        , Item 30 20
        ]

solve :: Int -> [Item] -> [Item]
solve volume items = items

main :: IO ()
main = do
    print $ solve 30 items