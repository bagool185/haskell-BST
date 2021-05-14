import InsertTests
import LookupTests
import DisplayTests
import DeleteTests
import PropertyTests

import BST
import Test.HUnit



main :: IO()
main = do 
    runTestTT $ TestList (
        insertTests ++
        deleteTests ++
        displayTests ++
        lookupTests
        )

    runPropertyTests 
    return()