module Tests where

import XMLParser ( parseXML, XML(..) )
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
        Just tree -> do
            putStrLn "----- PRINT XML -----"
            putStrLn $ printXML tree

            putStrLn "----- SELECT ATTR -----"
            print $ selectAttr "0" "id" tree  -- Should be Just "0"
            print $ selectAttr "1" "id" tree  -- Should be Just "1"
            print $ selectAttr "0" "type" tree -- Should be Nothing

            putStrLn "----- GET CHILDREN -----"
            print $ getChildren "0" tree 

            putStrLn "----- GET N-TH CHILD -----"
            print $ child "0" 0 tree
            print $ child "0" 1 tree 
            print $ child "0" 2 tree
            print $ child "0" 10 tree  