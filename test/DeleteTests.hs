module DeleteTests (deleteTests) where 
    
import Test.HUnit ( assertEqual, Test(TestLabel, TestCase) )
import BST ( delete, BST(Leaf, InternalNode) )
import Mocks ( mockTree )

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
    "it should delete the node and replace it with its right sub-tree's leftmost child"
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