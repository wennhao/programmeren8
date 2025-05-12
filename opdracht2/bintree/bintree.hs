module Main where

import Data.Char
import Data.List
import System.Environment

data Bintree a = Empty | Node a (Bintree a) (Bintree a) deriving (Show, Read)

push::(Ord a) => (Bintree a) -> a -> (Bintree a)
push Empty _value = Node _value Empty Empty
push (Node _current _left _right) _value = if _value <= _current then Node _current (push _left _value) _right else Node _current _left (push _right _value)

pushlist::(Ord a) => (Bintree a) -> [a] -> (Bintree a)
pushlist _top _values = foldl push _top _values

maptree::(a -> b) -> (Bintree a) -> (Bintree b)
maptree _ Empty = Empty
maptree _func (Node _value _left _right) = Node (_func _value) (maptree _func _left) (maptree _func _right)

filtertree::(a -> Bool) -> (Bintree a) -> [a]
filtertree _ Empty = []
filtertree _cond (Node _value _left _right) = filtertree _cond _left ++ (if _cond _value then [_value] else []) ++ filtertree _cond _right

preorder::(Bintree a) -> [a]
preorder Empty = []
preorder (Node _value _left _right) = [_value] ++ preorder _left ++ preorder _right

postorder::(Bintree a) -> [a]
postorder Empty = []
postorder (Node _value _left _right) = postorder _left ++ postorder _right ++ [_value]

inorder::(Bintree a) -> [a]
inorder Empty = []
inorder (Node _value _left _right) = inorder _left ++ [_value] ++ inorder _right

main::IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            text <- readFile file
            let chartree = pushlist Empty text --All characters into a new tree
            let inttree = maptree fromEnum chartree --Convert every character to ASCII
            writeFile "tree.txt" (show inttree) --Write ASCII tree to "tree.txt"
            
            readtree <- readFile "tree.txt" --Read "tree.txt" and load it into readtree
            let newtree = read readtree --newtree removes the "" from readtree
            let backtochartree = maptree chr newtree --Convert ASCII back to char
            let stringfromtree = concat [(inorder backtochartree)] --Concat the inorder list of all nodes
            putStrLn stringfromtree --Output the inorder string

            let onlynumbers = filtertree isDigit backtochartree --Filter numbers of the char tree
            putStrLn onlynumbers --Output the numbers
        _ -> putStrLn "Wrong number of arguments"