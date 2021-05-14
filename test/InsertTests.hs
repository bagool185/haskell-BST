module InsertTests (
  insertTests
  ) where


import Test.HUnit ( assertEqual, Test(TestLabel, TestCase) )
import BST ( insert, BST(Leaf, InternalNode) )

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