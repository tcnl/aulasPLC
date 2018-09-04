doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x*2

valoresIguais :: Int -> Int -> Int -> Bool
valoresIguais x y z = (x == y) && (y == z)

-- Guardas

maxi :: Int -> Int -> Int
maxi x y
    | x > y = x
    | otherwise = y

-- 21/08/2018

quadrado :: Int -> Int
quadrado x = x * x   
 
addD a b = 2 * (a+b)

maxIf :: Int -> Int -> Int
maxIf x y = (if x >= y then x else y) + 5

vendas 0 = 5
vendas 1 = 7
vendas 2 = 15
vendas 3 = 9

totalVendas :: Int -> Int
totalVendas n 
    | n == 0 = vendas 0 -- caso base
    | otherwise = vendas n + totalVendas (n - 1) -- caso recursivo

maxVendas :: Int -> Int
maxVendas n
    | n == 0 = vendas 0 -- caso base
    | otherwise = maxIf (vendas n) (maxVendas (n - 1)) -- caso recursivo

-- casamento de padrão

totalVendas2 :: Int -> Int
totalVendas2 0 = vendas 0
totalVendas2 n = vendas n + (totalVendas2 (n-1))

mynot :: Bool -> Bool
mynot True = False
mynot False = True

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd True False = False
myAnd False True = False
myAnd False False = False

myAnd2 :: Bool -> Bool -> Bool
myAnd2 True x = x
-- myAnd2 False x = False
myAnd2 False _ = False -- o _ (underline) torna o argumento indiferente

-- 23/08/2018



