
--Testversion für berechung rationaler Zahlen

data My_rational = Rat Integer Integer          -- Rationale Zahl

data Point = P (My_rational, My_rational)       -- Typendef für Koordinatenpunkt


get_N :: My_rational -> Integer
get_N (Rat z n) = n

get_Z :: My_rational -> Integer
get_Z (Rat z n) = z

print_number :: Int -> My_rational -> [Integer]     -- gibt Feld der Rationalen zahl mit eingebener Stellenanzahl aus
print_number _ (Rat _ 0) = []
print_number 0 _ = []
print_number i (Rat z n) = [(z `div` n)] ++ print_number (i-1) (Rat ((mod z n)*10) n)


-- Mathematische Operatoren für rationale Zahlen
mult :: My_rational -> My_rational -> My_rational
mult (Rat z1 n1) (Rat z2 n2) = reduce (Rat (z1*z2) (n1*n2))

divi :: My_rational -> My_rational -> My_rational
divi (Rat z1 n1) (Rat z2 n2) = reduce (Rat (z1*n2) (n1*z2))

addi :: My_rational -> My_rational -> My_rational
addi (Rat z1 n1) (Rat z2 n2) = reduce (Rat (z1*n2 + z2*n1) (n1*n2))

subs :: My_rational -> My_rational -> My_rational
subs (Rat z1 n1) (Rat z2 n2) = reduce (Rat (z1*n2 - z2*n1) (n1*n2))


-- Wurzelberechung mittels Liste
m_sqrt :: My_rational -> My_rational -> [My_rational]
m_sqrt x z = [z] ++ m_sqrt x (calc_s x z)
    where   calc_s :: My_rational -> My_rational -> My_rational
            calc_s a b = divi (addi b (divi a b)) (Rat 2  1)

-- Wurzelberechung ohne Liste mittels Newton-verfahren
m_s :: Int -> My_rational -> My_rational -> My_rational
m_s 0 x z = z
m_s i x z = m_s (i-1) x (calc_s x z)
    where   calc_s :: My_rational -> My_rational -> My_rational
            calc_s a b = divi (addi b (divi a b)) (Rat 2  1)
            
get_in_list :: Int -> [My_rational] -> My_rational
get_in_list 0 (x:xs) = x
get_in_list i (x:xs) = get_in_list (i-1) xs

-- potenzieren
power :: My_rational -> Integer -> My_rational
power x 1 = x
power x i = mult x (power x (i-1))

-- Fakutät
fak :: Integer -> Integer
fak 0 = 1
fak n = n * fak (n-1)

-- Multiplikation mit Integer
mult_int :: My_rational -> Integer -> My_rational
mult_int (Rat z1 n1) i = Rat (z1*i) (n1)

-- Division mit Integer
div_int :: My_rational -> Integer -> My_rational
div_int (Rat z1 n1) i = Rat (z1) (n1*i)

get_N_Z :: My_rational -> [Integer]
get_N_Z (Rat z n) = [z, n]

-- Sinus mittels Taylorreihe
m_sin :: Integer -> My_rational -> My_rational
m_sin (-1) x = Rat 0 1
m_sin i x = case (mod i 2) of
            1 -> subs (m_sin (i-1) x) ( div_int (power x  (2*i +1)) (fak (2*i +1)) )
            0 -> addi (m_sin (i-1) x) ( div_int (power x  (2*i +1)) (fak (2*i +1)) )



-- größter gemeinsamer Teiler
m_gcd :: Integer -> Integer -> Integer
m_gcd a b
    | a == b    = a
    | b > a     = gcd b a
    | otherwise = gcd (a-b) b 


-- kürzen des Bruchs
reduce :: My_rational -> My_rational
reduce (Rat z n) = Rat (div z d) (div n d)
    where   d = m_gcd (abs z) (abs n)
            abs :: Integer -> Integer
            abs i = if i<0 then -i else i


-- Simulationsschrit des gekickten Rotors
get_next :: Point -> Point
get_next (P (t, p)) = P (l, addi p (mult (Rat 13 5) (m_sin 5 l)))
    where l = addi p t

get_point :: Point -> [[Integer]]
get_point (P (t, p)) = [get_N_Z t] ++ [get_N_Z p]

-- startet simulationsberechung
calc :: Int -> Point -> [[[Integer]]]
calc 0 _ = [[[]]]
calc i cur_p = [ get_point cur_p ] ++ calc (i-1) (get_next cur_p)



main :: IO ()
main = do
    putStrLn . show $ calc 100 (P ((Rat 3 1), (Rat 1 2)))--print_number 100  (m_sin 10 (Rat 2 1)) --print_number 100 ((m_s 10 (Rat 2 1) (Rat 2 1)))
    

    
    
    
    
    
    
    
    
    
    
    
    
    
