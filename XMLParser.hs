module XMLParser ( 
    XML(..),
    Attr,
    parseXML,
) where 


import Data.Char ( isAlpha, isSpace )

-- XML Model

-- for intuition and readability
type Name = String
type Value = String
type Attr = (Name, Value)
type Id = String

data XML = Element {
    name :: Name, -- tag name
    attrs :: [Attr], -- list of attributes
    children :: [XML], -- nested tags
    text :: Maybe String, -- text inside the element
    uid :: Id -- unique id
} deriving (Show)

-- Parser Combinator paradigm

-- the parser function 
-- returns: Just (parsed_result, remaining_input) | Nothing
type Parser a = String -> Maybe (a, String)

-- char 'a' "abc" -> ('a', "bc")
char :: Char -> Parser Char
char _ [] = Nothing
char c (x : xs) 
    | c == x = Just (c, xs)
    | otherwise = Nothing

skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- "consumes" letters
parseName :: Parser String
parseName input = 
    let (name, rest) = span isAlpha input
    in 
        if null name then Nothing else Just (name, rest)

-- parses content of strings such as ""...""
parseQuoted :: Parser String
parseQuoted ('"' : xs) = 
    let (val, rest) = span (\c -> c /= '"') xs
    in case rest of 
        ('"' : r) -> Just (val, r)
        _ -> Nothing
parseQuoted _ = Nothing

-- parses an attribute, returns (key, value) pair if correct or Nothing
parseAttr :: Parser Attr
parseAttr input = do
    let input' = skipSpaces input
    (key, rest1) <- parseName input'
    (_,   rest2) <- char '=' rest1
    (val, rest3) <- parseQuoted rest2
    return ((key, val), rest3)

-- parses multiple attributes
parseAttrs :: Parser [Attr]
parseAttrs input =
    case parseAttr input of
        Just (a, rest) -> 
            case parseAttrs rest of 
                Just (as, rest') -> Just (a : as, rest')
                Nothing -> Just ([a], rest)
        Nothing -> Just ([], input)

-- parses open tag
-- eg: <name attr="value"> -> (tagName, attributes)+rest
parseOpenTag :: Parser (Name, [Attr])
parseOpenTag input = do
    (_, r1) <- char '<' input
    (name, r2) <- parseName r1
    (attrs, r3) <- parseAttrs r2
    (_, r4) <- char '>' r3
    return ((name, attrs), r4)

-- parses close tag </tag>
-- returns () because closing tag contains no data
parseCloseTag :: Name -> Parser ()
parseCloseTag tag input = do
    (_, r1) <- char '<' input
    (_, r2) <- char '/' r1
    (name, r3) <- parseName r2
    if name /= tag then Nothing else do
        (_, r4) <- char '>' r3
        return ((), r4)

-- reads everything until '<'
-- used for extracting the actual data of the element
parseText :: Parser String
parseText input =
    let (text, rest) = span (/= '<') input
    in if null text then Nothing else Just (text, rest)

-- parses child elements or text
parseContent :: String -> (([XML], Maybe String), String)
parseContent input =
  case parseElement input of
    Just (child, rest) ->
      let ((others, _), rest') = parseContent rest
      in ((child:others, Nothing), rest')
    Nothing ->
      case parseText input of
        Just (text, rest) -> (([], Just text), rest)
        Nothing       -> (([], Nothing), input)

makeId :: [Attr] -> Id
makeId attrs =
  case lookup "id" attrs of
    Just v  -> v
    Nothing -> "gen"

-- whole logic
parseElement :: Parser XML
parseElement input = do
    ((tag, attrs), r1) <- parseOpenTag (skipSpaces input)
    let ((children, text), r2) = parseContent r1
    (_, r3) <- parseCloseTag tag r2
    let elementId = makeId attrs
    return (Element tag attrs children text elementId, r3)

-- top level parser
parseXML :: String -> Maybe XML
parseXML input =
  case parseElement input of
    Just (xml, _) -> Just xml
    Nothing      -> Nothing