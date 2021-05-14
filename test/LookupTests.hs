module LookupTests (lookupTests) where 

import Test.HUnit
import BST 
import Mocks

lookupRoot :: Test
lookupRoot = TestCase (assertEqual
    "it should return the item with a matching key"
    (Just "root")
    (BST.lookup 5 mockTree)
    )

lookupLeaf :: Test
lookupLeaf = TestCase (assertEqual
    "it should return the item with a matching key"
    (Just "node2")
    (BST.lookup 2 mockTree)
    )

lookupInnerChild :: Test 
lookupInnerChild = TestCase (assertEqual
    "it should return the item with a matching key"
    (Just "node3")
    (BST.lookup 3 mockTree)
    )

lookupInexistentKey :: Test 
lookupInexistentKey = TestCase (assertEqual
    "it should return Nothing"
    Nothing 
    (BST.lookup 100 mockTree)
    )

lookupTests :: [Test]
lookupTests = [
    TestLabel "Looking up the value of root" lookupRoot,
    TestLabel "Looking up the value of a leaf" lookupLeaf,
    TestLabel "Looking up the value of a non-root non-leaf node" lookupInnerChild,
    TestLabel "Looking up the value of a inexistent node" lookupInexistentKey
    ]
