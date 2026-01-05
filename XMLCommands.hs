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
import XMLParser ( XML(..), Attr )

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

-- Finds element by it's ID
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

-- Returns attributes of element by given id
selectAttr :: String -> String -> XML -> Maybe String
selectAttr target key xml = do
    element <- findById target xml
    lookup key (attrs element)

-- Sets attribute of element with id
setAttr :: String -> String -> String -> XML -> XML
setAttr target key val (Element n attrs children text uid)
    | uid == target =
        Element n (updateAttr attrs) children text uid
    | otherwise =
        Element n attrs (map (setAttr target key val) children) text uid
  where
    updateAttr [] = [(key,val)]
    updateAttr ((k,v):xs)
        | k == key = (k,val):xs
        | otherwise = (k,v):updateAttr xs

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

-- Gets the text of element with given id
getText :: String -> XML -> Maybe String
getText target (Element _ _ children text uid)
    | uid == target = text
    | otherwise = first (map (getText target) children)
    where
        first :: [Maybe a] -> Maybe a
        first [] = Nothing
        first (x : xs) =
            case x of
                Just _ -> x
                Nothing -> first xs

-- Deletes an attribute of element with given id
deleteAttr :: String -> String -> XML -> XML
deleteAttr target key (Element name attrs children text uid)
    | uid == target = 
        Element name (filter ((/= key) . fst) attrs) children text uid
    | otherwise = Element name attrs (map (deleteAttr target key) children) text uid

-- Creates new child of element
newChild :: String -> String -> XML -> XML
newChild target childName (Element name attrs children text uid)
    | uid == target =
        Element name attrs (children ++ [emptyChild]) text uid
    | otherwise =
        Element name attrs (map (newChild target childName) children) text uid
  where
    emptyChild = Element childName [] [] Nothing "gen"

xpath = undefined