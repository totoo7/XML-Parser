module XPath (
    XPath,
    Step(..),
    XPathResult(..)
    parseXPath,
    evalXPath
) where

import XMLParser ( XML(..) )

data Step = 
    Child String 
    | ChildIndex String Int 
    | Attr String 
    | FilterEq Name String

data XPathResult
  = Node XML
  | TextValue String
  | AttrValue String
  deriving (Show)

type XPath = [Step]