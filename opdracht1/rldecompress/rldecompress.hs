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
            let pairs = (zip split (tail split))++[(head $ reverse split, "0")]
            let decomp = concat [if isDigit $ head $ fst z then (replicate ((read (fst z)::Int)-1) (head $ snd z)) else fst z | z <- pairs]
            writeFile output decomp
            
            putStrLn "File decompressed"
        _ -> putStrLn "Wrong number of arguments"

-- Cijfers in tekst gaat fout, want ze maken dan deel uit van de telling van het volgende character;
-- bvb: "33 appels" -> "23 a2pels" wat dus 23 spaties zou geven.