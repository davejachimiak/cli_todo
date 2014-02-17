module Todo.Help
    ( help
    , synopsis ) where

import Todo.UnknownCommand

help :: [String] -> IO ()
help ("add":_)    = putStrLn addUsage
help ("bump":_)   = putStrLn bumpUsage
help ("remove":_) = putStrLn removeUsage
help ("view":_)   = putStrLn viewUsage
help []           = synopsis
help (command:_)  = unknownCommand command

synopsis :: IO ()
synopsis = do
    putStrLn "general usage: todo [-h[command] | command arguments]"
    putStrLn ""
    putStrLn $ "specific usage: " ++ addUsage
    putStrLn $ "                " ++ bumpUsage
    putStrLn $ "                " ++ removeUsage
    putStrLn $ "                " ++ viewUsage

addUsage :: String
addUsage = usageString "add filepath \"task item\""

bumpUsage :: String
bumpUsage = usageString "bump filepath task_number"

removeUsage :: String
removeUsage = usageString "remove filepath task_number"

viewUsage :: String
viewUsage = usageString "view filepath"

usageString :: String -> String
usageString string = "todo " ++ string
