{-# OPTIONS_GHC -Wall #-}
module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- Input the number of discs and the names for three pegs.
-- Return a list of every moves.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 oriPeg resPeg _ = [(oriPeg, resPeg)]
hanoi size oriPeg resPeg temPeg = 
    hanoi (size-1) oriPeg temPeg resPeg
    ++ [(oriPeg, resPeg)]
    ++ hanoi (size-1) temPeg resPeg oriPeg