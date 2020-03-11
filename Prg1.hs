putDisplay :: (Int, String) -> IO ()
putDisplay (n, display) = putStrLn (show n ++ ": " ++ display)

prompt :: [String] -> IO ()
prompt displays = do
    putStrLn ""
    putStrLn "Current DISPLAY list:"
    mapM_ putDisplay (zip [0..] displays)
    command <- getLine
    interpret command displays

interpret :: String -> [String] -> IO ()
interpret ('+':' ':display) displays = prompt (display:displays)
interpret ('-':' ':num ) displays =
    case delete (read num) displays of
        Nothing -> do
            putStrLn "No DISPLAY entry matches the given number"
            prompt displays
        Just displays' -> prompt displays'
interpret  "q"          displays = return ()
interpret  command      displays = do
    putStrLn ("Invalid command: `" ++ command ++ "`")
    prompt displays

delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    as' <- delete (n - 1) as
    return (a:as')
delete _  []    = Nothing

main = do
    putStrLn "Commands:"
    putStrLn "+ <String> - Add a DISPLAY entry"
    putStrLn "- <Int>    - Delete the NUMBERED entry"
    putStrLn "q          - Quit"
    prompt []