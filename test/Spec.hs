import InsertTests ( insertTests )
import LookupTests ( lookupTests )
import DisplayTests ( displayTests )
import DeleteTests ( deleteTests )
import PropertyTests ( runPropertyTests )

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