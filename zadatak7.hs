-- 1)
data Stablo = 
    Nista 
    | Cvor Int Stablo Stablo
    deriving Show

-- 2)
-- let lista = Cvor 1 Nista Nista

sadrzi :: Int -> Stablo -> Bool 
sadrzi _ Nista = False
sadrzi v (Cvor val levo desno)
    | val == v = True
    | otherwise = sadrzi v levo || sadrzi v desno

-- 3)
uListu :: Stablo -> [Int]
uListu Nista = []
uListu (Cvor v levo desno) = uListu levo ++ [v] ++ uListu desno

-- 4)
brojeviUStablu :: Stablo -> [Int]
brojeviUStablu Nista = []
brojeviUStablu (Cvor c levo desno) 
    | mod c 3 == 0 || mod c 5 == 0 = [c] ++ brojeviUStablu levo ++ brojeviUStablu desno
    | otherwise                    = brojeviUStablu levo ++ brojeviUStablu desno
 

-- 5)
preslikaj :: Stablo -> Stablo
preslikaj Nista = Nista
preslikaj (Cvor c levo desno) = Cvor c (preslikaj desno) (preslikaj levo)  

-- 6)
f :: Int -> Bool 
f x
  | even x = True
  | otherwise = False

filterStablo :: (Int -> Bool) -> Stablo -> [Int]
filterStablo _ Nista = []
filterStablo funct (Cvor c desno levo)
    | funct c    = [c] ++ filterStablo f desno ++ filterStablo f levo
    | otherwise  = filterStablo f desno ++ filterStablo f levo 
        
