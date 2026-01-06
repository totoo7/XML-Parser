module XPath (
    XPath,
    Step(..),
    XPathResult(..),
    parseXPath,
    evalXPath
) where

import XMLParser ( XML(..) )

data Step = 
    Child String 
    | ChildIndex String Int 
    | ChildAttr String String 
    | FilterEq String String
  deriving (Show)

data XPathResult
  = Node XML
  | TextValue String
  | AttrValue String
  deriving (Show)

type XPath = [Step]

parseXPath :: String -> Maybe XPath
parseXPath input = 
  let parts = split '/' input
  in sequence (map parseStep parts)
evalXPath = undefined

split :: Char -> String -> [String]
split _ [] = []
split delimiter str = 
  let (first, rest) = span (/= delimiter) str
  in case rest of 
    [] -> [first]
    (_ : xs) -> first : split delimiter xs

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

-- evalStep :: Step -> [XML] -> [XML]
-- evalStep (Child name) nodes = concatMap (filterChildren name)

-- filterChildren :: String -> XML -> [XML]
-- filterChildren searchName (Element _ _ children _ _) = filter matchName children
--   where 
--     matchName (Element name _ _ _ _) = searchName == name
--    matchName _ = False