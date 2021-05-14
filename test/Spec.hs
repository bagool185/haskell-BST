import BST
import Test.HUnit


-- mock tree
--    5
--  3   6
-- 2 4
mockTree :: BST
mockTree = InternalNode 5 "root"
                    (InternalNode 3 "node3"
                        (InternalNode 2 "node2" Leaf Leaf)
                        (InternalNode 4 "node4" Leaf Leaf))
                    (InternalNode 6 "node6" Leaf Leaf)

insertNodeInEmptyTree :: Test
insertNodeInEmptyTree = TestCase (assertEqual
    "it should create a tree with the given node as root and 2 leafs as its children"
    (InternalNode 1 "test" Leaf Leaf)
    (BST.insert (InternalNode 1 "test" Leaf Leaf) (1, "test"))
    )

insertNodeSameKey :: Test
insertNodeSameKey = TestCase (assertEqual
    "it should update the value of the node with the same key"
    (InternalNode 1 "newTest" (InternalNode 2 "test2" Leaf Leaf) Leaf)
    (BST.insert (InternalNode 1 "oldTest" (InternalNode 2 "test2" Leaf Leaf) Leaf) (1, "newTest"))
    )

insertBiggerValue :: Test
insertBiggerValue = TestCase (assertEqual
    "it should insert a bigger value to the right"
    (InternalNode 1 "test" Leaf (InternalNode 2 "test2" Leaf Leaf))
    (BST.insert (InternalNode 1 "test" Leaf Leaf) (2, "test2"))
    )

insertSmallerValue :: Test
insertSmallerValue = TestCase (assertEqual
    "it should insert a smaller value to the left"
    (InternalNode 2 "test" (InternalNode 1 "test2" Leaf Leaf) Leaf)
    (BST.insert (InternalNode 2 "test" Leaf Leaf) (1, "test2"))
    )

insertTests :: [Test]
insertTests = [
    TestLabel "Insert node in an empty tree" insertNodeInEmptyTree,
    TestLabel "Insert a node having the same key as another node in the tree" insertNodeSameKey,
    TestLabel "Insert a node that has a bigger value than the only node in a tree" insertBiggerValue,
    TestLabel "Insert a node that has a lesser value than the only node in a tree" insertSmallerValue
    ]

deleteLeaf :: Test
deleteLeaf = TestCase (assertEqual
    "it should delete the leaf node with the given key from the tree"
    (InternalNode 1 "test" Leaf Leaf)
    (BST.delete (InternalNode 1 "test" Leaf (InternalNode 2 "test2" Leaf Leaf)) 2)
    )

deleteRoot :: Test
deleteRoot = TestCase (assertEqual
    "it should delete the root and set the leftmost item in the right subtree as the new root"
    (InternalNode 2 "right child" (InternalNode (-1) "left child" Leaf Leaf) Leaf)
        (BST.delete 
            (
                InternalNode 1 "original root"
                    (InternalNode (-1) "left child" Leaf Leaf)
                    (InternalNode 2 "right child" Leaf Leaf)
            )
            1
        )
    )

deleteRootOnlyRightChild :: Test
deleteRootOnlyRightChild = TestCase (assertEqual
    "it should delete the root and set the right child as the new root"
    (InternalNode 2 "right child" Leaf Leaf)
    (BST.delete (InternalNode 1 "original root"
                    Leaf
                    (InternalNode 2 "right child" Leaf Leaf)
                )
                1)
    )

deleteRootOnlyLeftChild :: Test
deleteRootOnlyLeftChild = TestCase (assertEqual
    "it should delete the root and set the left child as the new root"
    (InternalNode 2 "left child" Leaf Leaf)
    (BST.delete (InternalNode 3 "original root"
                    (InternalNode 2 "left child" Leaf Leaf)
                    Leaf
                )
                1)
    )

-- expected tree
--    5
--  4   6
-- 2 
deleteInnerChild :: Test
deleteInnerChild = TestCase (assertEqual
    "it should delete the node and replace it with its leftmost child"
    (InternalNode 5 "root"
        (
            InternalNode 4 "node4"
                (InternalNode 2 "node2" Leaf Leaf)
                Leaf
        )
        (InternalNode 6 "node6" Leaf Leaf) )

    (BST.delete mockTree 3)
    )

deleteInexistentKey :: Test
deleteInexistentKey = TestCase (assertEqual
    "it should not delete any node and return the original tree"
    mockTree
    (BST.delete mockTree 100)
    )

deleteTests :: [Test]
deleteTests = [
    TestLabel "Delete a leaf from a tree" deleteLeaf,
    TestLabel "Delete the root of a tree" deleteRoot,
    TestLabel "Delete the root of a tree without a left subtree" deleteRootOnlyRightChild,
    TestLabel "Delete a non-leaf and non-root node" deleteInnerChild,
    TestLabel "Delete a node whose key doesn't exist in the tree" deleteInexistentKey
    ]


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


testDisplayInOrder :: Test
testDisplayInOrder = TestCase (assertEqual
    "it should return a sorted list of strings"
    ["a", "b", "c"]
    (BST.displayInOrder
        (InternalNode 2 "b"
            (InternalNode 1 "a" Leaf Leaf)
            (InternalNode 3 "c" Leaf Leaf)
        )
    )
    )

displayInOrderEmptyTree :: Test
displayInOrderEmptyTree = TestCase (assertEqual
    "it should return an empty list"
    []
    (BST.displayInOrder BST.emptyBST)
    )

displayTests :: [Test]
displayTests = [
    TestLabel "Display the items in a tree as a sorted list of strings" testDisplayInOrder,
    TestLabel "Display the items in an empty tree" displayInOrderEmptyTree
    ]


main :: IO Counts
main = runTestTT $ TestList (
    insertTests ++ 
    deleteTests ++ 
    displayTests ++ 
    lookupTests
    )