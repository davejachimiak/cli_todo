import System.Environment
import System.IO
import System.Directory
import Control.Exception
import Data.List

import Todo.UnknownCommand

main = do
    arguments <- getArgs

    if length arguments > 0
        then dispatch (head arguments) (tail arguments)
        else synopsis

dispatch :: String -> [String] -> IO ()
dispatch "-h"     = help
dispatch "add"    = add
dispatch "bump"   = bump
dispatch "remove" = remove
dispatch "view"   = view
dispatch unknown  = unknownCommand unknown

help :: [String] -> IO ()
help ("add":_)      = putStrLn addUsage
help ("bump":_)     = putStrLn bumpUsage
help ("remove":_)   = putStrLn removeUsage
help ("view":_)     = putStrLn viewUsage
help []             = synopsis
help (unknown:args) = unknownCommand unknown args

addUsage :: String
addUsage = usageString "add filename \"task item\""

bumpUsage :: String
bumpUsage = usageString "bump filename task_number"

removeUsage :: String
removeUsage = usageString "remove filename task_number"

viewUsage :: String
viewUsage = usageString "view filename"

usageString :: String -> String
usageString string = "todo " ++ string

synopsis :: IO ()
synopsis = do
    putStrLn "general usage: todo [-h[command] | command arguments]"
    putStrLn ""
    putStrLn $ "specific usage: " ++ addUsage
    putStrLn $ "                " ++ bumpUsage
    putStrLn $ "                " ++ removeUsage
    putStrLn $ "                " ++ viewUsage

add :: [String] -> IO ()
add [filename, item] = appendFile filename (item ++ "\n")

bump :: [String] -> IO ()
bump [filename, numberString] = do
    rawItems <- readFile filename

    let (items, bumpedItem) = extractItem rawItems numberString
        newRawItems         = unlines $ bumpedItem:delete bumpedItem items

    overwriteRawItemsToFile filename newRawItems
bump _ = putStrLn "The bump command takes a file name and number as arguments."

extractItem :: String -> String -> ([String], String)
extractItem rawItems numberString =
    let number = read numberString :: Int
        items  = lines rawItems
    in (items, items !! number)

overwriteRawItemsToFile :: String -> String -> IO ()
overwriteRawItemsToFile filename rawItems = do
    bracketOnError (openTempFile "." "temp")
      (\(tempName, tempHandle) -> do
          hClose tempHandle
          removeFile tempName)
      (\(tempName, tempHandle) -> do
          hPutStr tempHandle rawItems
          hClose tempHandle
          removeFile filename
          renameFile tempName filename)

view :: [String] -> IO ()
view [filename] =
    let numerizeItem number item = (show number) ++ " -- " ++ item
    in do
      rawItems <- readFile filename

      let items         = lines rawItems
          numberedItems = zipWith numerizeItem [0..] items

      mapM_ putStrLn numberedItems
view _ = putStrLn "The view command takes a file name as an argument."

remove :: [String] -> IO ()
remove [filename, numberString] = do
    rawItems <- readFile filename

    let (items, removedItem) = extractItem rawItems numberString
        newRawItems = unlines $ delete removedItem items

    overwriteRawItemsToFile filename newRawItems
remove _ = putStrLn "The remove command takes a file name and a number as arguments."
