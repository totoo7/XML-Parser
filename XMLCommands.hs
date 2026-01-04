module XMLCommands (
    printXML,
    selectAttr,
    setAttr,
    getChildren,
    child,
    getText,
    deleteAttr,
    newChild,
    xpath
) where 
import XMLParser ( XML(..), Attr)

printXML :: XML -> String
printXML = printElement 0
  where
    -- number of spaces for indentation
    indent :: Int -> String
    indent level = replicate (level * 2) ' '

    -- print a single element recursively
    printElement :: Int -> XML -> String
    printElement level (Element elementName attributes childrenList maybeText _) =
        indent level ++ "<" ++ elementName ++ printAttrs attributes ++ ">\n" ++
        printChildren (level + 1) childrenList ++
        printText (level + 1) maybeText ++
        indent level ++ "</" ++ elementName ++ ">\n"

    -- print attributes as: key="value"
    printAttrs :: [Attr] -> String
    printAttrs [] = ""
    printAttrs attrs = " " ++ unwords [key ++ "=\"" ++ value ++ "\"" | (key, value) <- attrs]

    -- print all child elements
    printChildren :: Int -> [XML] -> String
    printChildren _ [] = ""
    printChildren level childrenList = concatMap (printElement level) childrenList

    -- print text content if it exists
    printText :: Int -> Maybe String -> String
    printText _ Nothing = ""
    printText level (Just t) = indent level ++ t ++ "\n"


findById :: String -> XML -> Maybe XML
findById target root@(Element _ _ childrenList _ _)
    | uid root == target = Just root
    | otherwise = searchChildren childrenList
  where
    searchChildren :: [XML] -> Maybe XML
    searchChildren [] = Nothing
    searchChildren (x:xs) =
        case findById target x of
          Just found -> Just found
          Nothing    -> searchChildren xs


selectAttr :: String -> String -> XML -> Maybe String
selectAttr target key xml = do
    element <- findById target xml
    lookup key (attrs element)

setAttr = undefined

-- Get list of attributes of child elements
getChildren :: String -> XML -> [[Attr]]
getChildren targetId xml =
    case findById targetId xml of
        Nothing -> []
        Just el -> map attrs (children el)

-- Get n-th child of element with given id
child :: String -> Int -> XML -> Maybe XML
child targetId n xml =
    case findById targetId xml of
        Nothing -> Nothing
        Just el -> if n < length (children el)
                    then Just (children el !! n)
                    else Nothing

getText = undefined
deleteAttr = undefined
newChild = undefined
xpath = undefined