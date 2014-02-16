import System.Environment
import System.IO
import System.Directory
import Data.List

main = do
  (command:args) <- getArgs
  dispatch command args

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
dispatch "remove" = remove
dispatch command  = unknownCommand command

add :: [String] -> IO ()
add [filename, item] = appendFile filename (item ++ "\n")

view :: [String] -> IO ()
view [filename] =
  let zipF n item = (show n) ++ " -- " ++ item
  in do
    rawItems <- readFile filename

    let items         = lines rawItems
        numberedItems = zipWith zipF [0..] items

    mapM_ putStrLn numberedItems

remove :: [String] -> IO ()
remove [filename, nString] = do
  rawItems <- readFile filename

  let n           = read nString :: Int
      items       = lines rawItems
      removedItem = items !! n
      newItems    = unlines $ delete removedItem items

  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newItems
  hClose tempHandle
  removeFile filename
  renameFile tempName filename

unknownCommand :: String -> [String] -> IO()
unknownCommand command _ = putStrLn $ "Unknown command: " ++ command
