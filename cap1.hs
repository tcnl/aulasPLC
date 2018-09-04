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

-- 04/09/2018
-- LISTAS: 
-- - são sempre valores do mesmo tipo
-- - os valores. são sempre escritos entre colchetes
-- *Main> 1 : 2 : 3 : []
-- [1,2,3]
-- *Main> 1 : 2 : [3]
-- [1,2,3]
-- *Main> [1 .. 7]
-- [1,2,3,4,5,6,7]
-- *Main> ['a' .. 'z']
-- "abcdefghijklmnopqrstuvwxyz"
-- *Main> [1, 2 .. 20]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-- *Main> [1, 3 .. 30]
-- [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29]
-- *Main> [2.8, 3.5 .. 9]
-- [2.8,3.5,4.2,4.9,5.6000000000000005,6.300000000000001,7.000000000000001,7.700000000000001,8.400000000000002,9.100000000000003]

somaLista :: [Int] -> Int
-- caso base
somaLista [] = 0
-- caso recursivo: lista tem cabeça e cauda
somaLista (x:xs) = x + somaLista xs

-- dobrar elementos de uma lista
double :: [Int] -> [Int]
double [] = []
double (x:xs) = (2 * x) : double xs


