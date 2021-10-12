import Data.Sequence
import Data.Foldable
--Imports require the use of Prelude. for some of the questions

{- *** Question 2.1 *** -}
pretty :: [[[Char]]] -> String
pretty xss
    = concat(concat (map insert xss))
insert :: [[Char]]-> [[Char]]
insert []
    = []
insert (x:xs)
    = (x ++ "\n") : insert xs

type Point
    = (Int, Int)

glider :: [Point]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

{- *** Question 2.2 *** -}
visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation sizex sizey []
    = []
visualisation sizex sizey (x:xs)
    = mapPoints x (Prelude.replicate sizey (Prelude.replicate sizex '.')) : visualisation sizex sizey xs

mapPoints :: [Point] -> [[Char]] -> [[Char]]
mapPoints [] grid
    = grid
mapPoints points []
    = []
mapPoints ((x,y):xys) grid
    = toList(update y (toList (update x '#' (fromList ((mapPoints xys grid)!! y)))) (fromList (mapPoints xys grid)))

{- *** Question 2.3 *** -}
evolution :: [Point] ->[[Point]]
evolution x
    = Prelude.take 9 (iterate (generation) x)

generation :: [Point]->[Point]
generation points
     = removeDuplicates ((Prelude.filter (flip twoAdjacent points) points) ++ (Prelude.filter (flip threeAdjacent points) [(x,y) | x <- [0..9], y <- [0..9]]))

surrounding :: Point -> [Point]
surrounding (x,y)
    = [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
threeAdjacent :: Point -> [Point] -> Bool
threeAdjacent (x,y) points
    |3 == Prelude.length (Prelude.filter (`elem` surrounding (x,y)) points) = True
    |otherwise = False

twoAdjacent :: Point -> [Point] -> Bool
twoAdjacent (x,y) points
    |2 == Prelude.length (Prelude.filter (`elem` surrounding (x,y)) points) = True
    |3 == Prelude.length (Prelude.filter (`elem` surrounding (x,y)) points) = True
    |otherwise = False

removeDuplicates :: [Point] -> [Point]
removeDuplicates []
    =[]
removeDuplicates (x:xs)
    |x `elem`xs = removeDuplicates xs
    |otherwise = x : removeDuplicates xs

inBounds :: Int->Int->[Point]->[Point]
inBounds x y []
    = []
inBounds maxx maxy ((x,y):xys)
    | 0 <= x && x <= maxx && 0 <= y && y <= maxy = (x,y): inBounds maxx maxy xys
    |otherwise = inBounds maxx maxy xys
main :: IO ()
main
    =  putStrLn (pretty (Prelude.take 8 (visualisation 5 5 (evolution glider))))
  
