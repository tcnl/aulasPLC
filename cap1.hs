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
    | n == 0 = vendas 0
    | otherwise = vendas n + totalVendas (n - 1)