module Tests where

import XMLParser ( parseXML, XML(..), assignUniqueIds )
import XMLCommands ( 
    printXML,
    selectAttr,
    getChildren,
    child,
    getText,
    setAttr,
    deleteAttr,
    newChild
    )

-- Example XML string
xml :: String
xml = "<people>\
        \<person id=\"0\">\
            \<name>John Smith</name>\
            \<address type=\"home\">Brooklyn, NY</address>\
            \<address type=\"work\">Manhattan, NYC</address>\
            \<occupation>dentist</occupation>\
        \</person>\
        \<person id=\"1\">\
            \<name>Ivan Petrov</name>\
            \<address>Bulgaria</address>\
        \</person>\
      \</people>"

-- Parse the XML once
maybeTree :: Maybe XML
maybeTree = parseXML xml

-- Run all tests
runTests :: IO ()
runTests =
    case maybeTree of
        Nothing -> putStrLn "XML parsing failed"
        Just t -> do
            let tree = assignUniqueIds t

            putStrLn "----- PRINT XML -----"
            putStrLn $ printXML tree

            putStrLn "----- SELECT ATTRIBUTE -----"
            print $ selectAttr "0" "id" tree
            print $ selectAttr "1" "id" tree
            print $ selectAttr "0" "type" tree

            putStrLn "----- GET CHILDREN -----"
            print $ getChildren "0_1" tree 

            putStrLn "----- GET N-TH CHILD -----"
            print $ child "0_1" 0 tree
            print $ child "0_1" 1 tree 
            print $ child "0_1" 2 tree
            print $ child "0" 10 tree  

            putStrLn "----- GET TEXT -----"
            print $ getText "5" tree
            print $ getText "2" tree

            putStrLn "----- SET ATTRIBUTE -----"
            let tree1 = setAttr "1" "type" "test" tree
            print $ selectAttr "1" "type" tree1

            putStrLn "----- DELETE ATTRIBUTE -----"
            let tree2 = deleteAttr "1" "type" tree1
            print $ selectAttr "1" "type" tree2  -- Should print: Nothing

            putStrLn "----- NEW CHILD -----"
            let tree3 = newChild "0_1" "test" tree
            putStrLn $ printXML tree3