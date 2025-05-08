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
            let sorted = sort text
            writeFile output sorted
            putStrLn "Text sorted"
        _ -> putStrLn "Wrong number of arguments"
