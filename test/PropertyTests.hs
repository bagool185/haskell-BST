{-# LANGUAGE TemplateHaskell #-}
module PropertyTests (runPropertyTests) where

import BST
import Test.HUnit
import Test.QuickCheck

import Data.Maybe


isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = (x <= y) && isSorted (y:xs)

isBST :: BST -> Bool
isBST Leaf = True
isBST tree = isSorted $ BST.displayInOrder tree

bst :: Gen BST
bst = boundedBST minBound maxBound

instance Arbitrary BST where 
    arbitrary = bst

boundedBST :: Int -> Int -> Gen BST
boundedBST low high =
    oneof [return Leaf,
        do
            key <- choose (low, high)
            leftChild <- boundedBST low (pred key)
            rightChild <- boundedBST (succ key) high
            return $ InternalNode key (show key) leftChild rightChild
        ]

insertLookup :: (Int, String) -> BST -> Bool
insertLookup (key, item) tree = isJust $ BST.lookup key $ BST.insert tree (key, item)

insertValid :: (Int, String) -> BST -> Bool 
insertValid (key, item) tree = isBST $ BST.insert tree (key, item)

prop_isBSTAfterDeletion :: Int -> BST -> Property 
prop_isBSTAfterDeletion key tree = classify (isJust $ BST.lookup key tree) "in tree" $ isBST $ BST.delete tree key

prop_canDelete :: Int -> BST -> Property 
prop_canDelete key tree = classify (isJust $ BST.lookup key tree) "in tree" $ isNothing $ BST.lookup key $ BST.delete tree key

return []
runPropertyTests = $verboseCheckAll 
