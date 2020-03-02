-- showResult :: Show a => a -> String
-- showResult a = "The result is " ++ (show a)

-- pi :: Floating a => a
-- pi = 3.141592653589793

-- -- showAreaOfCircle :: Num a => a -> String
-- showAreaOfCircle a = "The area of a circle with radius" ++ show a ++ "cm is about " ++ show (radius a) ++ "cm^2"
--     where radius x = Main.pi * x * x

-- sort2 :: Ord a => a -> a -> (a, a)
-- sort2 a b | a <= b       = (a, b)
--           | otherwise    = (b, a)

-- isLower :: Char -> Bool
-- isLower a   | a >= 'a' && a <= 'z' = True
--             | otherwise            = False

-- mangle :: String -> String
-- mangle (x:xs) = xs ++ [x]
-- mangle [] = []

-- length :: [a] -> Int
-- length [] = 0
-- length (x:xs) = 1 + Main.length xs

-- fact :: Int -> Int
-- fact 0 = 1
-- fact a = a * fact (a - 1)

-- countOdds :: [Int] -> Int
-- countOdds [] = 0
-- countOdds (x:xs) | odd x        = 1 + countOdds xs
--                  | otherwise    = countOdds xs

-- removeOdd :: [Int] -> [Int]
-- removeOdd [] = []
-- removeOdd (x:xs) | even x        = [x] ++ removeOdd xs
--                  | otherwise    = [] ++ removeOdd xs

-- myLast :: [a] -> a
-- myLast x = head $ reverse x

-- elementAt :: [a] -> Int -> a
-- elementAt x n = x !! (n - 1) 

-- myLength :: [a] -> Int
-- myLength (_:xs) = 1 + myLength xs
-- myLength [] = 0 

-- myLength' = foldr (\_ n -> 1 + n) 0

-- myReverse :: [a] -> [a]
-- myReverse = foldl (\x xs-> xs:x) []

-- isPalindrome :: Eq a => [a] -> Bool
-- isPalindrome x = x == (reverse x)

-- dupli :: [a] -> [a]
-- dupli = foldr (\x xs -> [x,x] ++ xs) []

-- repli :: [a] -> Int -> [a]
-- repli [] n = []
-- repli (x:xs) n = rep x n ++ repli xs n 
-- -- repli xs n = foldl (\acc e -> acc ++ rep e n) [] xs
--     where 
--         rep _ 0 = []
--         rep x n = x : rep x (n-1)

-- dropEvery :: [a] -> Int -> [a]
-- dropEvery xs n = snd $ foldl (\acc e -> if mod (fst acc) n == 0 then (fst acc + 1, snd acc) else (fst acc + 1, snd acc++[e])) (1,[]) xs

-- split :: [a] -> Int -> ([a], [a])
-- split xs n = helper (xs, []) n
--     where 
--         helper (xs, acc) 0 = (acc, xs)
--         helper (x:xs, acc) n = helper (xs, acc++[x]) (n - 1)

-- slice :: [a] -> Int -> Int -> [a]
-- -- slice xs n k = fst $ splitAt (k - n + 1) $ snd $ splitAt (n - 1) xs
-- slice xs n k = take (k - n + 1) $ drop (n - 1) xs

-- isPrime :: Integer -> Bool
-- isPrime 0 = False
-- isPrime 1 = False
-- isPrime 2 = True
-- isPrime x = helper x 2
--     where 
--         helper x i 
--             | x == i         = True
--             | x `mod` i /= 0 = helper x (i + 1)
--             | otherwise      = False

-- primeRange :: Integer -> Integer -> [Integer]
-- primeRange l u = foldr (\x acc -> if isPrime x then x:acc else acc) [] [l..u]



main = print $ primeRange 100000 1000000