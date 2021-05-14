module DisplayTests (displayTests) where
    
import Test.HUnit ( assertEqual, Test(TestLabel, TestCase) )
import BST ( displayInOrder, emptyBST, BST(Leaf, InternalNode) ) 

testDisplayInOrder :: Test
testDisplayInOrder = TestCase (assertEqual
    "it should return a list of strings sorted by their keys"
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
