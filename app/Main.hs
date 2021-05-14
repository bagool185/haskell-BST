module Main where

import BST

exampleTree :: BST
exampleTree = emptyBST

main :: IO ()
main = do
    let tree1 = BST.insert exampleTree (5, "hi")
        tree2 = BST.insert tree1 (3, "I'm")
        tree3 = BST.insert tree2 (-1, "a BST")
        tree4 = BST.delete tree3 3 in
        print (BST.displayInOrder tree4)

