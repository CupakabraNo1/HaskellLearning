
-- kvadrat a = a * a


-- 1)
ispeglaj:: [[Int]] -> [Int]
ispeglaj [] = []
ispeglaj (x:xs) = x ++ ispeglaj xs
-- ispeglaj xs = foldl (++) [] xs 

-- 2)

sumiraj:: [[Int]] -> [Int]
sumiraj [] = []
sumiraj (x:xs) = sumirajListu: sumiraj xs
    where
        sumirajListu = sum x

sumiraj2:: [[Int]] -> [Int]
sumiraj2 [] = []
sumiraj2 (x:xs) = suma x : sumiraj2 xs
    where
        suma = foldl (+) 0 

-- 3)

izbacitiParne:: [[Int]] -> [[Int]]
izbacitiParne [] = []
izbacitiParne (x:xs)
    | null neparniElementi = izbacitiParne xs
    | otherwise = neparniElementi : izbacitiParne xs
    where
        -- bezParnih = filter ( \x -> mod x 2 /= 0) x
        jeNeparan y = mod 2 y == 1
        neparniElementi = filter jeNeparan x

-- 4)
okreni:: [[Char]] -> [[Char]]
-- okreni [] = []
--  okreni (x: xs) = reverse x : okreni xs
--     where 
--         obrni = reverse
okreni = map reverse

-- 5)

izbaciDeljiveSaTri :: [[Int]] -> [[Int]]
izbaciDeljiveSaTri [] = []
izbaciDeljiveSaTri (x:xs) = filter jeDeljivSaTri x : izbaciDeljiveSaTri xs
    where
        jeDeljivSaTri y = mod 3 y == 1
        -- bezDeljivih = filter jeDeljivSaTri 

-- 6)

izbaciKraceOdPet :: [[Int]] -> [[Int]]
izbaciKraceOdPet [] = []
izbaciKraceOdPet l = filter duziOdCetiri (izbaciDeljiveSaTri l)
    where
        duziOdCetiri list = length list > 4