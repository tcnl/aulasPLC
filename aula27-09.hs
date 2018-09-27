type Nome = String
data Pessoa = M Nome | F Nome

instance Show Pessoa where
    show (M n) = n
    show (F n) = n

instance Eq Pessoa where
    (M n1) == (M n2) = n1 == n2
    (F n1) == (F n2) = n1 == n2
    _ == _= False

f :: Int -> Int -> Int
f a b = a + 10

g :: Int -> Int
g x = x + g x

troca :: Integer -> a -> a -> a
troca n x y
 | n > 0 = x
 | otherwise = y

h :: Int -> Int -> Int
h x y = x + y

fk :: [Int] -> [Int] -> Int
fk (x:xs) (y:ys) = x + ys
