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


-----------------------------------------------------------------------------
-- 21/08/2018
-----------------------------------------------------------------------------

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


-----------------------------------------------------------------------------
-- 23/08/2018
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- 04/09/2018
-----------------------------------------------------------------------------

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

-- saber se um elemento pertence a uma lista
member :: [Int] -> Int -> Bool
member [] n = False
member (x:xs) n = (x == n) || member xs n

-- é dígito
ehDigito :: Char -> Bool
ehDigito ch = (ch >= '0') && (ch <= '9')

digits :: [Char] -> [Char]
digits [] = []
digits (x:xs)
  | ehDigito x = x : digits xs
  | otherwise = digits xs

somaParesLista :: [(Int, Int)] -> [Int]
somaParesLista [] = []
somaParesLista ((a,b):xs) = (a+b) : somaParesLista xs

-----------------------------------------------------------------------------
-- 06/09/2018
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- 11/09/2018
-----------------------------------------------------------------------------
tamLista :: [t] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

-- Função zip pega o 1 elemento da 1 lista e faz par com o 1 elemento da 2 lista...
-- *Main> zip [1,2,3][1,2,3]
-- [(1,1),(2,2),(3,3)]
-- *Main> zip [True, False] [1,2]
-- [(True,1),(False,2)]

mzip :: [a] -> [b] -> [(a,b)]
mzip [] _ = []
mzip _ [] = []
mzip (x:xs) (y:ys) = (x,y) : mzip xs ys

mzip2 :: [a] -> [b] -> [(a,b)]
mzip2 (x:xs) (y:ys) = (x,y) : mzip2 xs ys
mzip2 _ _ = []

{-
mzip [1,2] [True, False]
= (1,True) : mzip [2] [False,False]
= (1,True) : (2:False): mzip [] []
= (1,True) : (2,False): []
= [(1,True),(2,False)]

mzip [1,2] [True, False, False]
= (1,True) : mzip [2,3] [False]
= (1,True) : (2:False): mzip [3] []
= (1,True) : (2,False): []
= [(1,True),(2,False)]
-}

-- Funções de alta ordem: funções que recebem outra função como argumento
inc :: Int -> Int
inc x = x + 1

aplicaDuasVezes :: (t -> t) -> t -> t
aplicaDuasVezes f x = f (f x)

-- repetiu a implementação da função "totalVendas2"

somaQuadrados :: Int -> Int
somaQuadrados 0 = quadrado 0
somaQuadrados n = quadrado n + somaQuadrados (n-1)

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = f n + total f (n-1)

-- *Main> total vendas 2
-- 27

-- repetiu a implementação da função "maxi"

maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0 
maxFun f n = maxi (f n) (maxFun f (n-1))

-- repetiu a implementação da função "double"

quadradoL :: [Int] -> [Int]
quadradoL [] = []
quadradoL (x:xs) = (x*x) : quadradoL xs

mapLInt :: (Int -> Int) -> [Int] -> [Int]
mapLInt f [] = []
mapLInt f (x:xs) = (f x) : mapLInt f xs

ehPar :: Int -> Bool
ehPar x = mod x 2 == 0

-- *Main> map ehPar [1..8]
-- [False,True,False,True,False,True,False,True]

-- *Main> map quadrado [1..8]
-- [1,4,9,16,25,36,49,64]

-- *Main> map (\x -> 2 * x) [1..8]
-- [2,4,6,8,10,12,14,16]

-- *Main> :t fst
-- fst :: (a, b) -> a

-- *Main> :t length
-- length :: Foldable t => t a -> Int

-- Folding

-- repetiu a implementação da função "somaLista"

disjL :: [Bool] -> Bool
disjL [] = False
disjL (x:xs) = (||) x (disjL xs)

-- *Main> disjL [True]
-- True
-- *Main> disjL [True,False]
-- True
-- *Main> disjL [False]
-- False
-- *Main> disjL []
-- False

-- *Main> :t (||)
-- (||) :: Bool -> Bool -> Bool
-- *Main> :t (+)
-- (+) :: Num a => a -> a -> a

mfold :: (t -> t -> t) -> [t] -> t
mfold f [x] = x
mfold f (x:xs) = f x (mfold f xs)
-- mfold eh foldrl no ghci

-- *Main> mfold (+) [1..6]
-- 21
-- *Main> mfold (||) [True]
-- True
-- *Main> mfold (||) [False]
-- False
-- *Main> mfold (||) [True, False]
-- True
-- *Main> mfold (||) []
-- *** Exception: /home/cinthya/aulasPLC/aulas.hs:(244,1)-(245,33): Non-exhaustive patterns in function mfold

mfold2 :: (t -> t -> t) -> t -> [t] -> t
mfold2 f v [] = v
mfold2 f v (x:xs) = f x (mfold2 f v xs)
-- mfold2 eh foldr no ghci

-- *Main> mfold2 (||) False []
-- False

-- *Main> foldr (+) 0 [1..5]
-- 15
-- *Main> foldr (*) 1 [1..5]
-- 120

-- *Main> foldr1 (*) []
-- *** Exception: Prelude.foldr1: empty list

mconcat :: [[t]] -> [t]
mconcat l = foldr1 (++) l

-- *Main> foldr1 (++) ["abc","def","ghi"]
-- "abcdefghi"
-- *Main> foldr1 (++) [[1..3],[4..6],[7..9]]
-- [1,2,3,4,5,6,7,8,9]
-- *Main> concat [[1..3],[4..6],[7..9]]
-- [1,2,3,4,5,6,7,8,9]

--Filtro

-- *Main> filter ehPar[1..10]
-- [2,4,6,8,10]
-- *Main> filter (not. ehPar) [1..10]
-- [1,3,5,7,9]

filtro :: (a -> Bool) -> [a] -> [a]
filtro f [] = []
filtro f (x:xs)
  | f x = x: filtro f xs
  | otherwise = filtro f xs 


-----------------------------------------------------------------------------
-- 13/09/2018
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- 18/09/2018
-----------------------------------------------------------------------------
inc' :: Int -> Int
inc' x = x + 1

twice :: (t -> t) -> t -> t
twice f x = f (f x)

-- *Main> twice inc 1
-- 3

iter :: Int -> (Int -> Int) -> (Int -> Int)
iter 0 f = id
iter n f = (iter (n-1) f) . f

-- *Main> (iter 3 inc) 0
-- 3
{-
iter 3 inc
= (iter 2 inc) . inc
= ((iter 1 inc). inc ). inc
= (((iter 0 inc) . inc). inc).inc
=((id . inc). inc). inc
-}

-- Notação lambda
{-
Inc : (\x -> x + 1)

*Main> (\x -> x + 1) 3
4
*Main> (\x y -> x + y) 3 8
11
-}

addNum n= (\m -> m + n)
-- *Main> :t addNum
-- addNum :: Num a => a -> a -> a
-- *Main> :t addNum 2
-- addNum 2 :: Num a => a -> a

-- *Main> (+) 2 3
-- 5
-- *Main> ((+) 2) 5
-- 7

multiplica :: Int -> Int -> Int
multiplica m n = m * n
-- *Main> multiplica 4 3
-- 12
-- *Main> map (multiplica 2) [20 .. 25]
-- [40,42,44,46,48,50]

tresIguais :: Int -> Int -> Int -> Bool
tresIguais m n p = (m == n) && (n == p)
-- *Main> tresIguais 2 3 4
-- False
-- *Main> ((tresIguais 2) 3) 4
-- False

inc1Lista1 l = map inc l
inc1Lista1_2 l = map (\x -> x + 1) l
-- f1 l = (filter (\y -> y > 15). map (\x ->))


-----------------------------------------------------------------------------
-- 20/09/2018
-----------------------------------------------------------------------------

-- *Main> (map.filter) (>5) [[1..7],[7..24]]
-- [[6,7],[7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]]

-- type Sinonimo = Tipo

type Nome = String

-- Tipos algebricos (data)

-- Enumeração
data Mbool = MTrue | MFalse

data DiasSemana = Dom | Seg | Ter | Qua | Qui | Sex | Sab

data Estacao = Verao | Outono | Inverno | Primavera

data Temperatura = Frio | Quente deriving Show

-- clima :: Estacao -> Temperatura
-- clima Verao = Quente
-- clima Outono = Quente

-- clima Primavera = Quente

clima :: Estacao -> Temperatura
clima Inverno = Frio
clima _ = Quente

-- *Main> clima Verao
-- Quente

-- Produto

type Idade = Int
data Pessoas = Pessoa Nome Idade deriving Show
-- *Main> :t Pessoa "Maria" 15
-- Pessoa "Maria" 15 :: Pessoas

exibirNome :: Pessoas -> String
exibirNome (Pessoa n i) = n
-- *Main> exibirNome (Pessoa "Maria" 15)
-- "Maria"

data Figura =   Circulo Float
                | Retangulo Float Float deriving Show

area :: Figura -> Float
area (Circulo r) = pi * r * r
area (Retangulo l h) = l * h
-- *Main> area (Circulo 2.0)
-- 12.566371
-- *Main> area (Retangulo 2.0 3.7)
-- 7.4

ehCircular :: Figura -> Bool
ehCircular (Circulo _) = True
ehCircular _ = False
-- *Main> ehCircular (Circulo 2.0)
-- True
-- *Main> ehCircular (Retangulo 2.0 3.7)
-- False

data Expr = Lit Int
    | Add Expr Expr
    | Sub Expr Expr deriving Show

eval :: Expr -> Int
eval (Lit n) = n
eval (Add exp1 exp2) = 
        eval exp1 + eval exp2
eval (Sub exp1 exp2) = 
        eval exp1 - eval exp2

-- *Main> eval (Add (Sub (Lit 5) (Lit 3)) (Lit 2))
-- 4

exibirExp :: Expr -> String
exibirExp (Lit n) = show n
exibirExp (Add e1 e2) =
    "( " ++ exibirExp e1 ++ " + " ++ exibirExp e2 ++ " )"
exibirExp (Sub e1 e2) = 
    "( " ++ exibirExp e1 ++ " - " ++ exibirExp e2 ++ " )"

data ListaInt = Vazia | Cons Int ListaInt deriving Show

somaListaInt :: ListaInt -> Int
somaListaInt (Vazia) = 0
somaListaInt (Cons n l) = n + somaListaInt l
-- *Main> somaListaInt (Cons 3 (Cons 4 ( Cons 5 Vazia)))
-- 12

-- Tipo algebrico polimorfico

data Lista t = Nil | Const t (Lista t) deriving Show

tamanho :: Lista t -> Int
tamanho (Nil) = 0
tamanho (Const _ l) = 1 + tamanho l
-- *Main> tamanho (Const 1 Nil)
-- 1
-- *Main> tamanho Nil
-- 0
-- *Main> tamanho (Const 1 (Const 2 Nil))
-- 2

data Arvore t = NilArv | No (Arvore t) t (Arvore t)
-- *Main> :t NilArv
-- NilArv :: Arvore t

