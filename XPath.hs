module XPath (
    XPath,
    Step(..),
    XPathResult(..),
    parseXPath,
    evalXPath,
    runXPath
) where

import XMLParser ( XML(..), trim )
import Data.Maybe ( mapMaybe )

data Step = 
    Child String -- eg: "person"
    | ChildIndex String Int -- eg: "person[0]"
    | ChildAttr String String -- eg: "person[@id]"
    | FilterEq String String -- not yet implemented
  deriving (Show)

data XPathResult
  = Node XML -- returns full XML element
  | TextValue String -- returns the text content
  | AttrValue String -- returns the attribute value
  deriving (Show)

type XPath = [Step]

-- parses string as list of steps (XPath)
parseXPath :: String -> Maybe XPath
parseXPath input = 
  let parts = split '/' input
  in sequence (map parseStep parts)

-- standard split by delimeter
split :: Char -> String -> [String]
split _ [] = []
split delimiter str = 
  let (first, rest) = span (/= delimiter) str
  in case rest of 
    [] -> [first]
    (_ : xs) -> first : split delimiter xs

-- parsing step logic, handles cases for step with no params, index and attribute
parseStep :: String -> Maybe Step
parseStep str =
  case break (== '[') str of
    (name, "") ->
      Just (Child name)
    (name, '[' : rest) ->
      case span (/= ']') rest of
        ('@':attr, "]") ->
          Just (ChildAttr name attr)
        (index, "]") ->
          case reads index of
            [(i, "")] -> Just (ChildIndex name i)
            _ -> Nothing
        _ -> Nothing

-- evaluates the result by a given step, returns XML values
evalStep :: Step -> [XML] -> [XML]
evalStep (Child name) nodes =
  concatMap (filterChildren name) nodes
evalStep (ChildIndex name index) nodes =
  concatMap selectIndexed nodes
  where
    selectIndexed xml =
      case filterChildren name xml of
        xs | index >= 0 && index < length xs -> [xs !! index]
        _ -> []
evalStep (ChildAttr name attr) nodes =
  concatMap selectAttrChild nodes
  where
    selectAttrChild xml =
      filter hasAttr (filterChildren name xml)
    hasAttr (Element _ attrs _ _ _) =
      any ((== attr) . fst) attrs
evalStep _ _ = []

-- returns all child nodes by given name
filterChildren :: String -> XML -> [XML]
filterChildren searchName (Element _ _ children _ _) = filter matchName children
  where 
    matchName (Element name _ _ _ _) = searchName == name

-- evaluates whole XPath, first Step is executed on the root
evalXPath :: XPath -> XML -> [XPathResult]
evalXPath [] _ = []
evalXPath steps root =
  case last steps of
    ChildAttr _ attr ->
      mapMaybe (getAttrValue attr) finalNodes
    _ ->
      map xmlToTextOrNode finalNodes
  where
    firstNodes = evalStep (head steps) [root]
    finalNodes = foldl (\ns s -> evalStep s ns) firstNodes (tail steps)

-- extracts text from element as XPathResult
xmlToTextOrNode :: XML -> XPathResult
xmlToTextOrNode (Element _ _ [] (Just txt) _) =
  TextValue (trim txt)
xmlToTextOrNode xml =
  Node xml

-- test function
xmlText :: XML -> Maybe String
xmlText (Element _ _ _ Nothing _) = Nothing
xmlText (Element _ _ _ (Just "") _) = Nothing
xmlText (Element _ _ _ (Just t) _) = Just t

-- get attribute value
getAttrValue :: String -> XML -> Maybe XPathResult
getAttrValue key (Element _ attrs _ _ _) =
  case lookup key attrs of
    Just v  -> Just (AttrValue v)
    Nothing -> Nothing

runXPath :: String -> XML -> IO ()
runXPath query tree =
  case parseXPath query of
    Nothing ->
      putStrLn $ "Invalid XPath: " ++ query
    Just xp -> do
      putStrLn $ "XPath: " ++ query
      print $ evalXPath xp tree