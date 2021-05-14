module Main where

import BST

exampleTree :: BST
exampleTree = emptyBST

main :: IO ()
main = do 
    let tree1 = BST.insert exampleTree (5, "pula") 
        tree2 = BST.insert tree1 (3, "mea")
        tree3 = BST.insert tree2 (-1, "ce")  
        tree4 = BST.delete tree3 3 in
        print (BST.lookup 3 tree4)

