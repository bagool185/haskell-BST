import BST
import Test.HUnit

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
    (BST.delete (InternalNode 1 "test" (InternalNode 2 "test2" Leaf Leaf) Leaf) 2)
    )

deleteRoot :: Test
deleteRoot = TestCase (assertEqual
    "it should delete the root and set the leftmost child as the new root"
    (InternalNode (-1) "leftmost child" Leaf (InternalNode 2 "bigger child" Leaf Leaf))
    (BST.delete (InternalNode 1 "original root" (InternalNode (-1) "leftmost child" Leaf Leaf) (InternalNode 2 "bigger child" Leaf Leaf)) 1)
    )

deleteInnerChild :: Test 
deleteInnerChild = TestCase (assertEqual
    "it should delete the node and replace it with its leftmost child"
    (InternalNode Int String BST BST)
    )

deleteTests :: [Test]
deleteTests = [
    TestLabel "Delete a leaf from a tree" deleteLeaf,
    TestLabel "Delete the root of a tree" deleteRoot
    ]


main :: IO Counts
main = runTestTT $ TestList (insertTests ++ deleteTests)