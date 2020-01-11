{-# OPTIONS_GHC -Wall #-}
module CreditCard where

---------- Exercise 1 ---------- 

-- Convert positive Integers to a reversed list of digits
toDigitsRev :: Integer -> [Integer] 
toDigitsRev x
    | x <= 0 = []
toDigitsRev x = (mod x 10) : toDigitsRev (div x 10)

-- Convert positive Integers to a list of digits
toDigits :: Integer -> [Integer]  
toDigits = reverse . toDigitsRev



---------- Exercise 2 ---------- 

-- Double every other number beginning from the left
doubleEveryOtherl :: [Integer] -> [Integer]
doubleEveryOtherl (x1:x2:xs) = x1 : (x2*2) : doubleEveryOtherl xs
doubleEveryOtherl xs = xs

-- Double every other number beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherl . reverse



---------- Exercise 3 ----------

-- Calculate the sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . (map toDigits)



---------- Exercise 4 ----------

-- Indicate whether an Integer could be a valid credit card number
validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits