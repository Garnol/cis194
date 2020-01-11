{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

------------------------------
--------- Exercise 1 ---------
------------------------------

-- Input a list and return a new list of every nth element of it
-- Example:
-- takeEveryNth 2 [1..10] == [2,4,6,8,10]
-- takeEveryNth 3 [1..10] == [3,6,9]
-- takeEveryNth 10 [1,2,3] == []
-- takeEveryNth 3 "Example" = "al"
-- takeEveryNth 0 [1..10] == []
-- takeEveryNth (-1) [1..10] == []
takeEveryNth :: [a] -> Int -> [a]
takeEveryNth xs n
    | n > 0 = map snd $ filter ((==) 1 . fst) $ zip flag xs
            where flag = (concat . repeat $ replicate (n-1) 0 ++ [1]) :: [Integer]
takeEveryNth _ _ = []

-- Input a list, and return a list of lists.
-- The nth list in the output should contain every nth element from the input list.
-- Example:
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
skips :: [a] -> [[a]]
skips xs = map (takeEveryNth xs) [1..genericLength xs]


------------------------------
--------- Exercise 2 ---------
------------------------------

-- Input a list, find all the local maxima in it and return them in order.
-- Example:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
    | x2 > x1 && x2 > x3 = x2 : localMaxima (x2:x3:xs)
    | otherwise = localMaxima (x2:x3:xs)
localMaxima _ = []


------------------------------
--------- Exercise 3 ---------
------------------------------

-- Construct a column of the histogram in line.
-- Input the index, number of spaces and stars('*').
-- Example:
-- buildLine 1 1 3 == " ***=1"
-- buildLine 5 6 7 == "      *******=5"
buildLine :: Integer -> Integer -> Integer -> String
buildLine index space stars = genericReplicate space ' ' ++
                             genericReplicate stars '*' ++
                             "=" ++
                             show index

-- Input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list.
-- Example:
-- histogram [1,1,1,5] == "*        \n*        \n*   *    \n=========\n123456789\n"
-- putStr $ histogram [1,1,1,5] ==
-- *        
-- *        
-- *   *    
-- =========
-- 123456789
-- histogram [1,4,5,4,6,6,3,4,2,4,9] == "   *     \n   *     \n   * *   \n******  *\n=========\n123456789\n"
-- putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--    *     
--    *     
--    * *   
-- ******  *
-- =========
-- 123456789
histogram :: [Integer] -> String
histogram xs = unlines . transpose . foldr build [] $ zip [1..] count
               where count = map (pred . genericLength) . group . sort $ xs ++ [1..9]
                     theMax = maximum count
                     build :: (Integer, Integer) -> [String] -> [String]
                     build (index, stars) acc = buildLine index (theMax-stars) stars : acc