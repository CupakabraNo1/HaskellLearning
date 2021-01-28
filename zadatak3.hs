-- 1)

data Element a
  = Prazan
  | Cvor a (Element a)
  deriving (Show)

-- 2)

kreirajMojuListu :: [Int] -> Element Int
kreirajMojuListu [] = Prazan
-- kreirajMojuListu (x:xs) = Cvor x (kreirajMojuListu xs)
kreirajMojuListu xs = foldr Cvor Prazan xs

-- 3)

duzinaListe :: Element a -> Int
duzinaListe Prazan = 0
duzinaListe (Cvor _ sledeci) = 1 + duzinaListe sledeci

-- 4)

uListi :: Eq a => a -> Element a -> Bool
uListi _ Prazan = False
uListi x (Cvor y sledeci)
  | x == y = True
  | otherwise = uListi x sledeci

-- 5)

data Planeta = Nista | Planeta
      { 
        ime :: String,
        prcnik :: Double,
        gasovita :: Bool
      } deriving (Show)

-- 6)

type Planete = [Planeta]

-- 7)

nadjiPlanetu :: String -> Planete -> Planeta
nadjiPlanetu _ [] = Nista
nadjiPlanetu imePlanete (x:xs)
    | ime x == imePlanete= x
    | otherwise = nadjiPlanetu imePlanete xs

-- 8)

nadjiGasovite :: Planete -> Planete
nadjiGasovite [] = []
nadjiGasovite (x:xs)
    | gasovita x = x : nadjiGasovite xs
    | otherwise  = nadjiGasovite xs