-- 1)
ukloniNti :: [a] -> Int -> [a]
ukloniNti l n = ukloni l n 1
    where 
        ukloni :: [a] -> Int -> Int -> [a] 
        ukloni [] _ _ = []
        ukloni (x:xs) n k 
            | mod k n == 0 = ukloni xs n (k+1)
            | otherwise    = x : ukloni xs n (k+1)

-- 2)
listaDelilaca :: Int -> [Int]
listaDelilaca n = delioci n [1..n]
    where
        delioci :: Int -> [Int] -> [Int]
        delioci _ [] = []
        delioci n (x:xs)
            | mod n x == 0 = x : delioci n xs
            | otherwise    = delioci n xs

-- [x | x <- [1..n], mod n x == 0]

-- 3)
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (manji xs) ++ [x] ++ quicksort (veci xs)
    where
        manji l = filter (> x) l
        veci l = filter (<= x) l


-- 4)
filter'' :: (Int -> Bool) -> [Int] -> [Int]
filter'' f l = [x | x <- l, f x]

-- 5)
sumiraj :: [Int] -> Int
sumiraj = foldl (+) 1

-- 6)
zip' :: [Int] -> [Int] -> [Int]
zip' [] [] = []
zip' l [] = l
zip' [] l = l
zip' (x:xs) (y:ys) = (x+y) : zip' xs ys

-- 7)
sumaCifara :: Int -> Int 
sumaCifara 0 = 0
sumaCifara n = mod n 10 + sumaCifara (n `div` 10)

sumaCifara' :: Int -> Int 
sumaCifara' 0 = 0
sumaCifara' n = sumaCifaraAcc n 0
    where
        sumaCifaraAcc acc 0 = acc
        sumaCifaraAcc acc n = sumaCifaraAcc (n `div` 10) (acc + mod n 10)

-- 8)
sumirajParne :: Int -> Int 
sumirajParne 0 = 0
sumirajParne n 
    |   even (mod n 10) = mod n 10 + sumirajParne (n `div` 10)
    |   otherwise       = sumirajParne (n `div` 10)