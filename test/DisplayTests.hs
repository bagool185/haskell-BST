module DisplayTests (displayTests) where
    
import Test.HUnit
import BST 

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