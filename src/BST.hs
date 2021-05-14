module BST (
  BST (..), 
  lookup, 
  emptyBST, 
  insert, 
  delete,
  displayInOrder
  ) where

import Prelude hiding (lookup)

-----------------------------------------------------

data BST = InternalNode Int String BST BST
         | Leaf
    deriving (Eq, Show)

-----------------------------------------------------

emptyBST :: BST
emptyBST = Leaf

isEmpty :: BST -> Bool
isEmpty Leaf = True
isEmpty _    = False

lookup :: Int -> BST -> Maybe String
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode key item leftChild rightChild)
  | soughtKey < key = lookup soughtKey leftChild
  | soughtKey > key = lookup soughtKey rightChild
  | otherwise = Just item

insert :: BST -> (Int, String) -> BST
insert Leaf (key, item) = InternalNode key item Leaf Leaf
insert (InternalNode key item leftChild rightChild) (newKey, newItem)
  -- keys are equal, update in place  
  | newKey == key = InternalNode newKey newItem leftChild rightChild
  -- insert to the left
  | newKey < key = InternalNode key item (insert leftChild (newKey, newItem)) rightChild
  -- insert to the right
  | newKey > key = InternalNode key item leftChild (insert rightChild (newKey, newItem))

delete :: BST -> Int -> BST
delete Leaf _ = Leaf
delete (InternalNode key item leftChild rightChild) soughtKey
  | key == soughtKey = deleteSubtreeRoot(InternalNode key item leftChild rightChild)
  | key > soughtKey = InternalNode key item (delete leftChild soughtKey) rightChild
  | key < soughtKey = InternalNode key item leftChild (delete rightChild soughtKey)

deleteSubtreeRoot :: BST -> BST
deleteSubtreeRoot (InternalNode key item Leaf Leaf) = Leaf
deleteSubtreeRoot (InternalNode key item Leaf rightChild) = rightChild
deleteSubtreeRoot (InternalNode key item leftChild Leaf) = leftChild
deleteSubtreeRoot (InternalNode key item leftChild rightChild) = InternalNode newKey newItem leftChild Leaf
                  where
                    (newKey, newItem) = getLeftMostElementOfRightSubtree rightChild

getLeftMostElementOfRightSubtree :: BST -> (Int, String)
getLeftMostElementOfRightSubtree (InternalNode key item Leaf _) = (key, item)
getLeftMostElementOfRightSubtree (InternalNode _ _ leftChild _) = getLeftMostElementOfRightSubtree leftChild

isLeaf :: BST -> Bool
isLeaf Leaf             = True
isLeaf InternalNode {}  = False

displayInOrder :: BST -> [String]
displayInOrder Leaf = []
displayInOrder (InternalNode key item leftChild rightChild) = displayInOrder leftChild ++ [item] ++ displayInOrder rightChild

rotateRight :: BST -> BST
rotateRight (InternalNode keyB itemB (InternalNode keyA itemA alpha beta) gamma)
    = InternalNode keyA itemA alpha (InternalNode keyB itemB beta gamma)


-----------------------------------------------------

