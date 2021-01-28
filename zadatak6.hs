import Data.Char (isLower, toUpper)

-- 1)
kvadrirajIliDeset :: [Int] -> [Int]
kvadrirajIliDeset [] = []
kvadrirajIliDeset n
    |even (length n) = map (^2) n 
    |otherwise     = map (*10) n

-- 2)
obradiString :: String -> String 
obradiString str = map toUpper (izbaciVelika str)
    where
        izbaciVelika = filter isLower 

-- 3)

primeni :: ([[Int]] -> [[Int]]) -> ([[Int]] -> [[Int]]) -> [[Int]] -> [[Int]]
primeni _ _ [] = []
primeni f1 f2 l = f2 (f1 l)

-- 4)
--

-- 5)
prosecnaDuzina :: [String] -> Int
prosecnaDuzina [] = 0
prosecnaDuzina l = div suma (length l) 
    where
        duzine = map length l
        suma = sum duzine