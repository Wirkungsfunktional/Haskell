
module My_rational_numb
( My_rational(..)
, mult                  -- Rat * Rat
, mult_int              -- Rat * Int
, divi                  -- Rat / Rat
, div_int               -- Rat / Int
, addi                  -- Rat + Rat
, subs                  -- Rat - Rat
, print_number          -- Rat -> String
, m_s                   -- Sqrt(Rat)
, m_atan                -- atan(Rat)
, e_diff                -- extrapolated differentiation
, c_diff                -- central differentiation
, front_diff            -- forward differentiation
, from_float            -- Rat construct by float-number
) where

type Precision = Integer


data My_rational = Rat Integer Integer         -- rational number: z -> numerator, n -> denominator


-- make fraction to decimal string
print_number :: Precision -> My_rational -> String     
print_number i (Rat z n) = (show (z `div` n)) ++ "." ++ print_rest (i-1) (Rat ((mod z n)*10) n)
    where   print_rest :: Precision -> My_rational -> String
            print_rest _ (Rat _ 0) = ""
            print_rest 0 _ = ""
            print_rest i (Rat z n) = (show (z `div` n)) ++ print_rest (i-1) (Rat ((mod z n)*10) n)



-- math operators for fraction
mult :: My_rational -> My_rational -> My_rational
mult (Rat z1 n1) (Rat z2 n2) = reduce (Rat (z1*z2) (n1*n2))

divi :: My_rational -> My_rational -> My_rational
divi (Rat z1 n1) (Rat z2 n2) = reduce (Rat (z1*n2) (n1*z2))

addi :: My_rational -> My_rational -> My_rational
addi (Rat z1 n1) (Rat z2 n2) = reduce (Rat (z1*n2 + z2*n1) (n1*n2))

subs :: My_rational -> My_rational -> My_rational
subs (Rat z1 n1) (Rat z2 n2) = reduce (Rat (z1*n2 - z2*n1) (n1*n2))



-- square-root by newton-approach
m_s :: Precision -> My_rational -> My_rational -> My_rational
m_s 0 x z = z
m_s i x z = m_s (i-1) x (calc_s x z)
    where   calc_s :: My_rational -> My_rational -> My_rational
            calc_s a b = divi (addi b (divi a b)) (Rat 2  1)


-- exponentiate: number -> Exponent -> result
power :: My_rational -> Integer -> My_rational
power x 1 = x
power x i = mult x (power x (i-1))

-- factorial
fak :: Integer -> Integer
fak 0 = 1
fak n = n * fak (n-1)

-- Rat * Int
mult_int :: My_rational -> Integer -> My_rational
mult_int (Rat z1 n1) i = Rat (z1*i) (n1)

-- Rat / Int
div_int :: My_rational -> Integer -> My_rational
div_int (Rat z1 n1) i = Rat (z1) (n1*i)


-- return [Rat.z, Rat.n]
get_N_Z :: My_rational -> [Integer]
get_N_Z (Rat z n) = [z, n]

-- sine by taylorexpansion
m_sin :: Integer -> My_rational -> My_rational
m_sin (-1) x = Rat 0 1
m_sin i x = case (mod i 2) of
            1 -> subs (m_sin (i-1) x) ( div_int (power x  (2*i +1)) (fak (2*i +1)) )
            0 -> addi (m_sin (i-1) x) ( div_int (power x  (2*i +1)) (fak (2*i +1)) )

-- atan by taylorexpansion in range 0 to 2 (no precision over 2)
m_atan :: Precision -> My_rational -> My_rational
m_atan (-1) x = Rat 0 1
m_atan i x = case (mod i 2) of
            1 -> subs (m_atan (i-1) x) ( div_int (power x  (2*i +1)) (2*i +1) )
            0 -> addi (m_atan (i-1) x) ( div_int (power x  (2*i +1)) (2*i +1) )


-- greatest common divisor
m_gcd :: Integer -> Integer -> Integer
m_gcd a b
    | a == b    = a
    | b > a     = gcd b a
    | otherwise = gcd (a-b) b 


-- cancel of fraction
reduce :: My_rational -> My_rational
reduce (Rat z n) = Rat (div z d) (div n d)
    where   d = m_gcd (abs z) (abs n)
            abs :: Integer -> Integer
            abs i = if i<0 then -i else i

-- initiate fraction from float-number
from_float :: Float -> My_rational
from_float z = reduce $ Rat (round (z*(10.0^(11)))) (10^11)



-- Subclass fractions for better usage in context
instance Show My_rational where
    show x = print_number 50 x
    
instance Eq My_rational where
    (Rat z n) == (Rat a b) = (z==a)&&(n==b)

instance Num My_rational where
    a + b = addi a b
    a * b = mult a b
    a - b = subs a b


 

-- differentiation by forward
front_diff :: My_rational -> (My_rational -> My_rational) -> My_rational -> My_rational
front_diff x f h = divi ((f (x+h)) - (f x)) (h)

-- differentiation by central
c_diff :: My_rational -> (My_rational -> My_rational) -> My_rational -> My_rational
c_diff x f h = divi ((f (x+ (div_int h 2))) - (f (subs x (div_int h 2)))) (h)

-- differentiation by extrapolation
e_diff :: My_rational -> (My_rational -> My_rational) -> My_rational -> My_rational
e_diff x f h = divi (subs (mult_int ( subs (f (x+ (div_int h 4))) (f (subs x (div_int h 4)))) 8) (subs (f (x+ (div_int h 2))) (f (subs x (div_int h 2)))) ) (mult_int h 3)



    

    
    
    
    
    
    
    
    
    
    
    
    
    
