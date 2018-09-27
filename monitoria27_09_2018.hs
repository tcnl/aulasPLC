import Data.List(sort)
-- Questão 1 EE1 2018.1
-- Defina operadores de seção s1 e s2 de maneira que
-- filter (>5) . map (+2)
-- sol.:   x + 2 > 5
--         x > 5 - 2
--         x >= 3
-- então:
-- f = filter (>5) . map (+2)
-- f [1..5]
-- [6,7]
-- g = map (+2) . filter (>3)
-- g [1..5]
-- [6,7]

-- Questão 2 EE1 2018.1
-- a)
data CInt = Conjunto [Int] deriving (Show)

getInts :: CInt -> [Int]
getInts (Conjunto a) = a

makeSet :: [Int] -> CInt
makeSet [] = Conjunto []
makeSet (x:xs) = Conjunto . sort $ if x `notElem` xs then x:l else l
    where l = getInts (makeSet xs)

-- *Main> makeSet [11,11,3,4,5,2,2,3]
-- Conjunto [2,3,4,5,11]


