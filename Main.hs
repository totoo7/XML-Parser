module Main where

import System.IO
import XMLParser ( parseXML, XML(..) )
import XMLCommands (
    printXML,
    selectAttr,
    setAttr,
    getChildren,
    child,
    getText,
    deleteAttr,
    newChild
    )

main :: IO ()
main = loop Nothing  -- Nothing = no XML loaded yet
  where
    loop :: Maybe XML -> IO ()
    loop maybeXml = do
        putStr "> "
        hFlush stdout
        cmdLine <- getLine
        let args = words cmdLine
        case args of

            ["help"] -> do
                putStrLn "Available commands:"
                putStrLn "  open <filename> - Open XML file."
                putStrLn "  close - Close current XML file."
                putStrLn "  save <filename> - Save XML to file."
                putStrLn "  saveas <filename> - Save XML to a new file."
                putStrLn "  print - Print current XML."
                putStrLn "  select <id> <key> - Get attribute value."
                putStrLn "  set <id> <key> <value> - Set attribute."
                putStrLn "  children <id> - List attributes of children."
                putStrLn "  child <id> <n> - Get n-th child element."
                putStrLn "  text <id> - Get text of element."
                putStrLn "  delete <id> <key> - Delete attribute."
                putStrLn "  newchild <id> - Add new child."
                putStrLn "  exit - Quit program."
                loop maybeXml

            ["exit"] -> putStrLn "Exiting..." >> loop Nothing

            ["open", filename] -> do
                content <- readFile filename
                case parseXML content of
                    Nothing -> putStrLn "Failed to parse XML." >> loop Nothing
                    Just t  -> putStrLn "XML loaded successfully." >> loop (Just t)

            ["close"] -> putStrLn "XML closed." >> loop Nothing

            ["print"] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t  -> putStrLn $ printXML t
                >> loop maybeXml

            ["save", filename] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t  -> writeFile filename (printXML t) >> putStrLn "Saved."
                >> loop maybeXml

            ["saveas", filename] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t  -> writeFile filename (printXML t) >> putStrLn "Saved."
                >> loop maybeXml

            ["select", elId, key] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t  ->
                        print $ selectAttr elId key t
                >> loop maybeXml

            ["children", elId] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t  -> print $ getChildren elId t
                >> loop maybeXml

            ["child", elId, nStr] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t  ->
                        let n = read nStr :: Int
                        in print $ child elId n t
                >> loop maybeXml

            ["set", elId, key, val] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t -> 
                        let newTree = setAttr elId key val t
                        in loop (Just newTree)
                >> loop maybeXml

            ["delete", elId, key] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t -> 
                        let newTree = deleteAttr elId key t
                        in loop (Just newTree)
                >> loop maybeXml

            ["newchild", elId, childName] ->
                case maybeXml of
                    Nothing -> putStrLn "No XML loaded."
                    Just t -> 
                        let newTree = newChild elId childName t
                        in loop (Just newTree)
                >> loop maybeXml

            _ -> putStrLn "Unknown command, type 'help'." >> loop maybeXml