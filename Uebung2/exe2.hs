import Prelude hiding (unwords, words)

-- Aufgabe 1 a)
pack :: [Char] -> [[Char]]
pack [] = []
pack (x:xs) = (doWhile (==x) (x:xs)) : pack (pullWhile (==x) xs)
  where doWhile :: (Char -> Bool) -> [Char] -> [Char]
        doWhile _ [] = []
        doWhile p (x:xs) = if p x then x: (doWhile p xs) else []
        pullWhile :: (Char -> Bool) -> [Char] -> [Char]
        pullWhile _ [] = []
        pullWhile p (x:xs) = if p x then pullWhile p xs else (x:xs)


encode l = map (\x -> (length x, head x) ) (pack l)

decode :: [(Int, Char)] -> [Char]
decode [] = []
decode ((n, l):xs) = (list n l) ++ (decode xs)
  where list :: Int -> Char -> [Char]
        list 0 _ = []
        list n x = x : (list (n-1) x)


-- inefficient
rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate l 0 = l
rotate (x:xs) n = rotate (xs ++ [x]) (mod (n - 1) ((length xs) + 1) )

-- more efficient




unwords :: [String] -> String
unwords [] = []
unwords (x:xs) = x ++ " " ++ (unwords xs)

pack2 :: [Char] -> [[Char]]
pack2 [] = []
pack2 (x:xs) = (doWhile (/=' ') (x:xs)) : pack2 (pullWhile (/=' ') (xs)) -- hier substitute bedingung
  where doWhile :: (Char -> Bool) -> [Char] -> [Char]
        doWhile _ [] = []
        doWhile p (x:xs) = if p x then x: (doWhile p xs) else []
        pullWhile :: (Char -> Bool) -> [Char] -> [Char]
        pullWhile _ [] = []
        pullWhile p (x:xs) = if p x then pullWhile p xs else (xs) -- hier deleted x


words :: [Char] -> [[Char]]
words [] = []
words (x:xs) = map reverse $ aux [x] xs
  where aux sorted [] = [sorted]
        aux (x:sorted) (y:unsorted)
          | y/=' ' = aux (y:x:sorted) (unsorted)
          | otherwise =  (x:sorted) : aux [head unsorted] (tail unsorted)


--max_length = max $ map length


main :: IO ()
main = putStrLn $ show $ words "hallo Welt"--rotate [1,2,3,4] (-2) --decode $ encode ['a', 'a', 'b', 'b', 'b', 'a', 'a', 'a']
