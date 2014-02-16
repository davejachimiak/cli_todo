import System.Environment
import System.IO
import System.Directory
import Control.Exception
import Data.List

main = do
    (command:arguments) <- getArgs
    dispatch command arguments

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "bump"   = bump
dispatch "view"   = view
dispatch "remove" = remove
dispatch command  = unknownCommand command

add :: [String] -> IO ()
add [filename, item] = appendFile filename (item ++ "\n")
add _ = putStrLn "The add command takes a file name and a task as arguments."

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

unknownCommand :: String -> [String] -> IO()
unknownCommand command _ = putStrLn $ "Unknown command: " ++ command
