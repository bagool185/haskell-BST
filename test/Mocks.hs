module Mocks(mockTree) where

import BST ( BST(..) )

mockTree :: BST
mockTree = InternalNode 5 "root"
                    (InternalNode 3 "node3"
                        (InternalNode 2 "node2" Leaf Leaf)
                        (InternalNode 4 "node4" Leaf Leaf))
                    (InternalNode 6 "node6" Leaf Leaf)