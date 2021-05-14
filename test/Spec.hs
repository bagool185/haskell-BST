import InsertTests
import LookupTests
import DisplayTests
import DeleteTests
import BST
import Test.HUnit
import Test.QuickCheck ( choose, oneof, Gen )

import Data.Maybe

-- isBST :: BST -> Bool 
-- isBST = isBST' Nothing Nothing t 
--     where 
--         isBST' lower upper Leaf = True
--         isBST' lower upper (InternalNode key item leftChild rightChild) = 
--             maybeBounded lower upper (key, item) &&
--             isBST' lower (Just (key, item)) leftChild &&
--             isBST' (Just (key, item)) upper rightChild
        
--         maybeBounded Nothing Nothing (key, item) = True 
--         maybeBounded Nothing (Just upper) (key, item) = key < upper
--         maybeBounded (Just lower) Nothing (key, item) = lower < key 
--         maybeBounded (Just lower) (Just upper) (key, item) = lower < key && key < upper

bst :: Gen BST
bst = boundedBST minBound maxBound


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


-- insertValid :: (Int, String) -> BST -> Bool 
-- insertValid (key, item) tree = isBST $ BST.insert tree (key, item)


main :: IO Counts
main = runTestTT $ TestList (
    insertTests ++ 
    deleteTests ++ 
    displayTests ++ 
    lookupTests
    )