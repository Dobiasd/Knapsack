{-

Achja, ich hab mir ne lustige Aufgabe auf der Arbeit rausgesucht.
Auf der Arbeit hatte ich ja analysiert, was unsere Maschinen (Pharma, Kommissionierung) so für Vorteile bringen.
Einer ist dieser:
Sagen wir mal, so eine Apotheke bekommt 2 Bestellungen rein.

Bestellung_1:
Artikel;Anzahl
A;3
B;5

Bestellung_2:
Artikel;Anzahl
B;2
C;10

Normalerweise würde sich der Kommissionierer nun eine Kiste für Bestellung 1 nehmen, mit der Bestellung durchs Lager gehen und 3*A und 5*B einsammeln.
Dann nimmt er sich eine zweite Kiste, geht wieder durchs Lager und sammelt 2*B und 10*C.

Mit unserer Anlage bekommt er aber nur eine Liste
Artikel;Anzahl
A;3
B;7
C;10

Damit geht er dann einmal durchs Lager, legt den Kram aufs Band, er läuft unter unserer Kamera her, wird erkannt und vom Sorter in die passenden Kisten geschubbst.
Die Zeit, die man durchschnittlich pro Objekt braucht, ist dann natürlich geringer, weil man weniger Artikelwechsel hat, und ein Artikelwechsel Zeit kostet (hingehen, im Regen suchen usw.). Die Zeiten dafür hab ich auch aus Logs geparsed (Listen werden auf Handhelds angezeigt), aber das ist erstmal egal.

Wenn die beiden Bestellungen oben zu einem Batch zusammengefasst werden, werden aus vorher 4 Bestellzeilen 3 Batchzeilen. Die Einsparung davon nennen wir Batcheffekt.
In dem Fall wär Batcheffekt = (4-3)/4 = 25%.

Je größer der Sorter ist, den man von uns kauft, desto mehr Bestellungen kann man in einem Batch packen, womit dann auch der Batcheffekt wächst.
Batcheffekt in Abhängigkeit der Anzahl Sorter-Ausgänge ist dann ne Kurve, die am Anfang gut steigt und dann immer flacher von unten gegen Batcheffet 100% asymptotiert.

Naja, auf jeden Fall überlassen wir dem Kunden das Zusammenstellen der Batche. Wir zeigen alle, die eingegangen sind an, und er kann die sich dann in Batche zusammenklicken. Dabei zeigen wir den erreichten Batcheffekt an, helfen aber nicht algorithmisch, den zu maximieren.

Es gibt zwar noch ein paar Randbedingungen, aber vereinfacht will ich jetzt folgendes Problem angehen:
So ein Sorter hat oft 40 Ausgänge und pro Tag kommen 100 Bestellungen im Schnitt mit so 10 Bestellzeilen pro Bestellung rein.
Jetzt würd ich gern eine möglichst gute Batchzusammenstellung berechnen. Natürlich geht nicht die allerbeste weil NP und so.

Erstmal hab ichs mit 10000 Zufallszusammenstellungen probiert. Die schlechteste hatte nen Batcheffekt von 39% über den ganzen Tag, die beste eine von 43%; also nicht so ein großer Unterschied.
Aber 10000 ist ja nur ein suuuuuuperkleiner Teil von allen denkbaren Zusammenstellungen. Die Anzahl Protonen im Universum ist ja nix dagegen.

Die Artikel, die bestellt werden, sind nicht gleichverteilt, falls das hilft. Einige Sachen werden ja auf vielen Stationen ständig benötigt, andere ganz selten.
-}




{-
orders :: [Order]
orders = [ [1,2,3] -- a
         , [1,2]   -- b
         , [3,4]   -- c
         , [5]     -- d
         , [6,7,3] -- e
         ]


  abcde
a x1101
b  x251
c   x14
d    x4
e     x


[ [a, b] -- 1,2,3
, [c, e] -- 3,4,6,7
, [d]    -- 5
]

    x   x x   x
1 8 7 2 3 6 9 4
7 3 6 4

[ [a, b] -- 1,2,3
, [c, d] -- 3,4,5
, [e]    -- 6,7,3
]


-- groups with max m (m<=n) sets
-- []

-}


module Main where

{-| todo bla -}

import qualified Data.Set as Set
import Data.List
import Data.Function
import Text.Printf

{-| forward application -}
(-:) :: a -> (a -> b) -> b
x -: f = f x
infixl 0 -:

{-| combinations 2 "ABCD" -> ["AB","AC","AD","BC","BD","CD"] -}
combinations :: (Eq a, Num a) => a -> [a1] -> [[a1]]
combinations 0 lst = [[]]
combinations n lst = do
    (x:xs) <- tails lst
    rest   <- combinations (n-1) xs
    return $ x : rest

{-| allCombinations "ABC" -> ["A","B","C","AB","AC","BC","ABC"] -}
allCombinations :: [a] -> [[a]]
allCombinations l = map (\k -> combinations k l) [1..length l] -: concat

type Article = String

data Order = Order { name :: String
                   , numBoxes :: Int
                   , articles :: Set.Set Article } deriving Show

newtype Batch = Batch [Order]

showStats :: Batch -> String
showStats batch = printf "boxes: %d\n\
                          \order lines: %d\n\
                          \batch lines: %d\n\
                          \saved lines: %d\n\
                          \batch effect: %.1f%%"
                         (boxes batch)
                         (orderLines batch)
                         (batchLines batch)
                         (savedLines batch)
                         (100 * batchEffect batch)

instance Show Batch where
    show batch@(Batch orders) =
        intercalate "\n" $
        (map show orders) ++ [showStats batch]

{-| Sum of the original order lines in a batch. -}
orderLines :: Batch -> Int
orderLines (Batch orders) = orders -: map articles -: map Set.size -: sum

{-| Number of order lines after combining orders to a batch. -}
batchLines :: Batch -> Int
batchLines (Batch orders) = orders -: map articles -: Set.unions -: Set.size

{-| Number of order lines saved by combining orders to a batch. -}
savedLines :: Batch -> Int
savedLines batch = orderLines batch - batchLines batch

{-| Batch effect of a batch: Share of saved order lines. -}
batchEffect :: Batch -> Double
batchEffect batch
    | ol == 0 = 0
    | otherwise = fromIntegral (savedLines batch) / fromIntegral ol
    where ol = orderLines batch

{-| Number of boxes a batch needs. -}
boxes :: Batch -> Int
boxes (Batch orders) = orders -: map numBoxes -: sum

{-| Rate a batch by its saved lines.
    Return -1 if the batch is too large for the given number of boxes. -}
rateBatch :: Int -> Batch -> Int
rateBatch numBoxes batch
    | boxes batch > numBoxes = -1
    | otherwise = savedLines batch

{-| Find the best possible batch by brute force. -}
bestBatchBruteForce :: Int -> [Order] -> Batch
bestBatchBruteForce numBoxes orders =
    maximumBy (compare `on` (rateBatch numBoxes)) batches
    where
        combis = allCombinations orders
        batches = map Batch combis

{-| Find the best possible batch by ... -}
bestBatchHeuristic :: Int -> [Order] -> Batch
bestBatchHeuristic numBoxes orders = undefined

{-|
kind of a knapsack problem:
Construct the batch with a max of numBoxes boxes
that has the maximum possible saved lines.
-}
bestBatch :: String -> Int -> [Order] -> Batch
bestBatch "bruteForce" numBoxes orders = bestBatchBruteForce numBoxes orders
bestBatch "heuristic" numBoxes orders = bestBatchHeuristic numBoxes orders
bestBatch mode _ _ = error $ printf "unknown mode: %s" mode

{-| test input -}
testOrders :: [Order]
testOrders = [ Order "A" 2 $ Set.fromList ["1", "2", "3"]
             , Order "B" 1 $ Set.fromList ["1", "2"]
             , Order "C" 1 $ Set.fromList ["3", "4"]
             , Order "D" 2 $ Set.fromList ["5"]
             , Order "E" 1 $ Set.fromList ["6", "7", "3"]
             ]

main :: IO ()
main = do
    print $ bestBatch "bruteForce" 4 testOrders
    putStrLn "----------------------"
    print $ bestBatch "heuristic" 4 testOrders