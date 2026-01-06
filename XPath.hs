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
    | Attr String 
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
    (name, "") -> Just (Child name)
    (name, '[' : rest) ->
      case span (/= ']') rest of
        (index, "]") ->
          case reads index of
            [(i, "")] -> Just (ChildIndex name i)
            _ -> Nothing
    _ -> Nothing