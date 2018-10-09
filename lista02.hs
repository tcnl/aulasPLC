f:: [Int] -> [Int]
f[x] = []
f(x:y:zs) | x==y      = x : f (y:zs)
          | otherwise = f (y:zs)

{--
g::[Int] -> Bool
g x
    |a = filter (>1) x
    |a = filter (<101) a
    |a = map (%2) a
    |
--}
