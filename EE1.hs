-- https://wiki.haskell.org/99_questions/1_to_10
{-
1 Problem 1
(*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}
myLast :: [a] -> a
myLast [] = error "Lista vazia"
myLast [x] = x
myLast (_:xs) = myLast xs
-- *Main> myLast [1,2,3]
-- 3

{-
2 Problem 2
(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}

myButLast :: [a] -> a
myButLast [] = error "Lista vazia"
myButLast [x] = error "Há um só elemento"
myButLast (x:xs) =  if length xs == 1 then x
                    else myButLast xs

{-
3 Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.

Example:

* (element-at '(a b c d e) 3)
c
Example in Haskell:

Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
-}

elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt (_:xs) i = elementAt xs (i - 1)
elementAt _ _      = error "Index out of bounds"
-- *Main> elementAt [4,3,2,1] 3
-- 2

{-
4 Problem 4
(*) Find the number of elements of a list.

Example in Haskell:

Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
-}

myLength:: [a] -> Int
myLength [] = 0
-- myLength [x] = 1
myLength (_:xs) = 1 + myLength xs
-- *Main> myLength [1..5]
-- 5
-- *Main> myLength [3..5]
-- 3 
-- *Main> myLength [3..20]
-- 18

{-
5 Problem 5
(*) Reverse a list.

Example in Haskell:

Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
-}

myReverse :: [a] -> [a]
myReverse