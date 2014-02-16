import System.Environment
import System.IO
import System.Directory
import Control.Exception
import Data.List

import Todo.UnknownCommand
import Todo.Help

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

add :: [String] -> IO ()
add [filepath, item] = appendFile filepath (item ++ "\n")
add _ = wrongNumberOfArguments

bump :: [String] -> IO ()
bump [filepath, numberString] = do
    rawItems <- readFile filepath

    let (items, bumpedItem) = extractItem rawItems numberString
        newRawItems         = unlines $ bumpedItem:delete bumpedItem items

    overwriteRawItemsToFile filepath newRawItems
bump _ = wrongNumberOfArguments

view :: [String] -> IO ()
view [filepath] =
    let numerizeItem number item = (show number) ++ " -- " ++ item
    in do
        rawItems <- readFile filepath

        let items         = lines rawItems
            numberedItems = zipWith numerizeItem [0..] items

        mapM_ putStrLn numberedItems
view _ = wrongNumberOfArguments

remove :: [String] -> IO ()
remove [filepath, numberString] = do
    rawItems <- readFile filepath

    let (items, removedItem) = extractItem rawItems numberString
        newRawItems = unlines $ delete removedItem items

    overwriteRawItemsToFile filepath newRawItems
remove _ = wrongNumberOfArguments

extractItem :: String -> String -> ([String], String)
extractItem rawItems numberString =
    let number = read numberString :: Int
        items  = lines rawItems
    in (items, items !! number)

overwriteRawItemsToFile :: String -> String -> IO ()
overwriteRawItemsToFile filepath rawItems = do
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle rawItems
            hClose tempHandle
            removeFile filepath
            renameFile tempName filepath)

wrongNumberOfArguments = putStrLn "Wrong number of arguments. Try the `-h' option for usage."
