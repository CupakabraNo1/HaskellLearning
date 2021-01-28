
-- 1)

rastaviString :: Char -> String -> [String]
rastaviString _ [] = []
rastaviString delimiter l = rastavi delimiter l []
    where
        rastavi :: Char -> String -> String -> [String]
        rastavi _ [] acc = [acc]
        rastavi c (y:ys) acc
            | y == c    = acc : rastavi c ys []
            | otherwise = rastavi c ys (acc ++ [y])

-- 2) 

spojiListu :: [String] -> String
-- spojiListu = foldl spoji ""
--     where
--         spoji [] b = b
--         spoji a [] = a
--         spoji a b = a ++ ", " ++ b
spojiListu [] = ""
spojiListu [x] = x
spojiListu (x:xs) = x ++ "," ++ spojiListu xs

-- 3)

obradiListuStringova :: [String] -> String 
obradiListuStringova [] = ""
obradiListuStringova l = spojiListu rastavljeni
    where 
        rastavljeni = concatMap (rastaviString ' ') l

-- 4)

svastaSaListom :: [[Int]] -> Int 
svastaSaListom l = product sume
    where
        kvadrirani = map (map ( ^ 2) ) 
        sume = map sum (kvadrirani l)


-- 5)

data Naselje =
    Selo {
        broj_stanovnika :: Int,
        povrsina :: Double,
        tip :: String
    } 
    | Varosica {
        broj_stanovnika :: Int,
        povrsina :: Double
    }
    | Grad {
        broj_stanovnika :: Int,
        povrsina :: Double,
        ima_bazen :: Bool 
    }
    deriving Show

-- 6)
type Naselja = [Naselje]

izdvojOdredjene :: Naselja -> Naselja
izdvojOdredjene [] = []
izdvojOdredjene (x:xs)
    | jeSelo x && (tip x == "razbijeno")                       = x : izdvojOdredjene xs
    | jeGrad x && ima_bazen x && (broj_stanovnika x > 15000)   = x : izdvojOdredjene xs
    | otherwise                                                = izdvojOdredjene xs 
    where
        jeSelo Selo {} = True 
        jeSelo _       = False 
        jeGrad Grad {} = True
        jeGrad _       = False 