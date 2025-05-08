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
            putStrLn ("length of text.txt: "++(show $ length text)++" characters")
            let grouped = group text
            let compressed = concat [if length x > 1 then (show $ length x)++[head x] else [head x] | x <- grouped]
            putStrLn ("length of compressed file compressed.txt: "++(show $ length compressed)++" characters")
            putStrLn ("factor: "++(show (100.0 - (fromIntegral $ length compressed) / (fromIntegral $ length text)*100))++"%")
            writeFile output compressed
            
            putStrLn "File compressed"
        _ -> putStrLn "Wrong number of arguments"

-- Cijfers in tekst gaat fout, want ze maken dan deel uit van de telling van het volgende character;
-- bvb: "33 appels" -> "23 a2pels" wat dus 23 spaties zou geven.