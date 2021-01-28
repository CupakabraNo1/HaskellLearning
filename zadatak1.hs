-- saberi :: Int -> Int -> Int
-- saberi a b = a + b

-- oduzmi a b = a - b

-- pomnozi a b = a * b

-- fja 5 = 1
-- fja n = n * 2

-- funkcija :: [[Char]] -> [Char]
-- funkcija [] = ""
-- funkcija (x : xs) = x

-- flista [] = 0
-- flista [x] = 1
-- flista (x:xs) = 1 + flista xs

-- 1)
zadnjiElement :: [Int] -> [Int]
zadnjiElement [] = []
zadnjiElement [x] = []
zadnjiElement (x : xs) = x : zadnjiElement xs

-- 2)
ukloniPretposlednji :: [Int] -> [Int]
ukloniPretposlednji [] = []
ukloniPretposlednji [x, y] = [y]
ukloniPretposlednji (x : xs) = x : ukloniPretposlednji xs

-- 3)

-- a)
faktorijelRek :: Int -> Int
faktorijelRek 0 = 1
faktorijelRek n = n * faktorijelRek (n -1)

-- b)
faktorijelRep :: Int -> Int
-- faktorijelRep 0 = 1
faktorijelRep n = faktorijelPom n 1

faktorijelPom 0 akumulirajuciP = akumulirajuciP
faktorijelPom n akumulirajuciP = faktorijelPom (n -1) (akumulirajuciP * n)

-- 4)

imaVelikaSlova :: [Char] -> Bool
imaVelikaSlova [] = False
imaVelikaSlova (x : xs)
  | x >= 'A' && x <= 'Z' = True
  | otherwise = imaVelikaSlova xs

-- 5)

spljosti :: [Int] -> [Int]
spljosti [] = []
spljosti [x] = [x]
spljosti (x : y : xs)
  | x == y = spljosti (x : xs)
  | otherwise = x : spljosti (y : xs)

-- 6)

izbaciMalaSlova :: [[Char]] -> [[Char]]
izbaciMalaSlova [] = []
izbaciMalaSlova (x : xs)
  | imaVelikaSlova x = x : izbaciMalaSlova xs
  | otherwise = izbaciMalaSlova xs

-- 7)

kvadriraj :: [Int] -> [Int]
kvadriraj l = [x * x | x <- l]

-- 8)

jeDeljiv :: Int -> Int -> Bool
jeDeljiv 0 _ = False
jeDeljiv a b = mod b a == 0

-- 9)

jeDeljivSaTri :: Int -> Bool
jeDeljivSaTri = jeDeljiv 3

-- 10)

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' f [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

-- 11)

deljiviSaTri:: [Int] -> [Int]
deljiviSaTri = filter' jeDeljivSaTri
