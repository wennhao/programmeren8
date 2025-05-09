module Main where

import Data.Char
import Data.List
import System.Environment

main::IO ()
main = do
    args <- getArgs
    case args of
        [file, output] -> do
            text <- readFile file
            let split = groupBy (\x y -> isDigit x == isDigit y) text
            -- Groups adjacent numbers or characters (e.g. ["123","a","6","b"])
            let pairs = (zip split (tail split))++[(head $ reverse split, "0")]
            -- Makes pairs of adjacent elements and adds "0" to end (e.g. [("123","a"),("a","6"),("6","b"),("b","0")])
            let decomp = concat [if isDigit $ head $ fst z then (replicate ((read (fst z)::Int)-1) (head $ snd z)) else fst z | z <- pairs]
            -- If first element of tuple is a number x, then repeat character in second element x-1 times, otherwise return the character in first element
            -- (e.g. ("123","a") -> first elem = number -> replicates "a" 123-1=122 times. ("a","6") -> first elem = char -> return "a")
            writeFile output decomp
            
            putStrLn "File decompressed"
        _ -> putStrLn "Wrong number of arguments"

-- Cijfers in tekst gaat fout, want ze maken dan deel uit van de telling van het volgende character;
-- bvb: "33 appels" -> "23 a2pels" wat dus 23 spaties zou geven.