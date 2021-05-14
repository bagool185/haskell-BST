{-# LANGUAGE TemplateHaskell #-}
module PropertyTests (runPropertyTests) where

import BST ( delete, insert, lookup, BST(..), displayInOrder )
import Test.HUnit ()
import Test.QuickCheck
    ( quickCheckAll,
      choose,
      oneof,
      classify,
      Arbitrary(arbitrary),
      Gen,
      Property )

import Data.Maybe ( isJust, isNothing )


maxInt :: BST -> Int 
maxInt Leaf = minBound
maxInt (InternalNode key item leftChild rightChild) = max (max key $ maxInt leftChild) (max key $ maxInt rightChild)

minInt :: BST -> Int 
minInt Leaf = maxBound
minInt (InternalNode key item leftChild rightChild) = min (min key $ minInt leftChild) (min key $ minInt rightChild)

isBST :: BST -> Bool 
isBST Leaf = True
isBST (InternalNode key item Leaf Leaf) = True
isBST (InternalNode key item leftChild rightChild) = 
    maxInt leftChild < key && 
    minInt rightChild > key && 
    isBST leftChild && 
    isBST rightChild

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
prop_isBSTAfterInsertion :: (Int, String) -> BST -> Bool 
prop_isBSTAfterInsertion (key, item) tree = isBST $ BST.insert tree (key, item)

prop_isBSTAfterDeletion :: Int -> BST -> Property 
prop_isBSTAfterDeletion key tree = classify (isJust $ BST.lookup key tree) "in tree" $ isBST $ BST.delete tree key

prop_canDelete :: Int -> BST -> Property 
prop_canDelete key tree = classify (isJust $ BST.lookup key tree) "in tree" $ isNothing $ BST.lookup key $ BST.delete tree key

prop_canInsert :: (Int, String) -> BST -> Property 
prop_canInsert (key, item) tree = classify (isNothing $ BST.lookup key tree) "in tree" $ isJust $ BST.lookup key $ BST.insert tree (key, item)

return []
runPropertyTests = $quickCheckAll  
