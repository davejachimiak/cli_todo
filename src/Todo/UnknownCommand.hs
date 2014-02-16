module Todo.UnknownCommand where

unknownCommand :: String -> [String] -> IO ()
unknownCommand command _ = putStrLn $ "Unknown command: " ++ command
