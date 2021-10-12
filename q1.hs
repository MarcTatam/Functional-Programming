{- *** Used for Question 1.1 *** -}
unique :: [Int] -> Bool
unique []
    = True
unique (x:xs)
    | (x `elem` xs) == True = False
    | otherwise = unique xs

{- *** Used for Question 1.2 *** -}
evenOdd :: [Int] -> Bool
evenOdd []
    = True
evenOdd [x]
    =True
evenOdd (x:y:xs)
    |(x+y) `mod` 2 == 0 = False
    |otherwise = evenOdd (y:xs)

{- *** Used for Question 1.3 *** -}
twoDifference :: [Int] -> Bool
twoDifference []
    = True
twoDifference [x]
    = True
twoDifference (x:y:xs)
    |(x-y) <= 2 && (x-y) >= -2 =False
    |otherwise = twoDifference (y:xs)

{- *** Used for Question 1.1,1.2,1.3 *** -}
tupleToList :: (Int,Int,Int,Int,Int,Int) -> [Int]
tupleToList (a,b,c,d,e,f)
    = [a,b,c,d,e,f]

{- *** Question 1.1 *** -}
rule1 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule1 xs
    = unique (tupleToList xs)

{- *** Question 1.2 *** -}
rule2 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule2 xs
    = evenOdd (tupleToList xs)

{- *** Question 1.3 *** -}
rule3 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule3 xs
    = twoDifference (tupleToList xs)

{- *** Question 1.4 *** -}
rule4 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule4 (a,b,c,d,e,f)
    |(a*10+b) `mod`(e*10+f) == 0 && (c*10+d) `mod`(e*10+f) == 0 = True
    |otherwise = False

{- *** Question 1.5 *** -}
possibles :: [(Int,Int,Int,Int,Int,Int)]
possibles
    = [(a,b,c,d,e,f)|a<-[0..9],
                      b<-[0..9],
                      c<-[0..9],
                      d<-[0..9],
                      e<-[0..9],
                      f<-[0..9]]

{- *** Question 1.6 *** -}
isSolution :: (Int,Int,Int,Int,Int,Int) -> Bool
isSolution xs
    |rule1 xs == False = False
    |rule2 xs == False = False
    |rule3 xs == False = False
    |rule4 xs == False = False
    |otherwise = True

{- *** List of valid tuples *** -}
main :: IO ()
main =  putStrLn (show(filter (isSolution) (possibles)))
