-- chapter 3

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- chapter 4

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5" 

-- factorial2 :: (Integral a) => a -> a
factorial2 0 = 1
factorial2 n = n * factorial (n - 1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

first :: (a,b,c) -> a
first (x,_,_) = x
second :: (a,b,c) -> b
second (_,y,_) = y
third :: (a,b,c) -> c
third (_,_,z) = z

-- *Main> let xs = [(1,3), (4,3), (2,4)]
-- *Main> [a+b | (a,b) <- xs]
-- [4,7,6]

head' :: [a] -> a
head' [] = error "Cant't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y
-- *Main> tell [1,2,3,4,5]
-- "The list is long. The first two elements are: 1 and 2"
-- *Main> tell [1]
-- "The list has one element: 1"

length' :: (Num b) => [a] -> b
length' [] = 0
length'(_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- padrão
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
   | bmi <= 18.5 = "You're underweight, you emo, you!"
   | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
   | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
   | otherwise = "You're a whale,congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
    | weight / height ^ 2  <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale,congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

    -- Não funcionou
-- myCompare :: (Ord a) => a -> a -> a
-- -- a 'myCompare' b 
-- -- a ‘myCompare‘ b
-- -- a 'myCompare' b
--     | a > b = GT
--     | a == b = EQ
--     | otherwise = LT