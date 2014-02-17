module Todo.UnknownCommand where

unknownCommand :: String -> IO ()
unknownCommand command = putStrLn $ "Not a command: " ++ command
