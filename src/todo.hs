import System.Environment
import System.IO
import System.Directory
import Data.List

main = do
  (command:arguments) <- getArgs
  dispatch command arguments

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
dispatch "remove" = remove
dispatch command  = unknownCommand command

add :: [String] -> IO ()
add [filename, item] = appendFile filename (item ++ "\n")

view :: [String] -> IO ()
view [filename] =
  let numerizeItem number item = (show number) ++ " -- " ++ item
  in do
    rawItems <- readFile filename

    let items         = lines rawItems
        numberedItems = zipWith numerizeItem [0..] items

    mapM_ putStrLn numberedItems

remove :: [String] -> IO ()
remove [filename, numberString] = do
  rawItems <- readFile filename

  let number      = read numberString :: Int
      items       = lines rawItems
      removedItem = items !! number
      newRawItems = unlines $ delete removedItem items

  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newRawItems
  hClose tempHandle
  removeFile filename
  renameFile tempName filename

unknownCommand :: String -> [String] -> IO()
unknownCommand command _ = putStrLn $ "Unknown command: " ++ command
