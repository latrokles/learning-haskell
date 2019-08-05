main = do
    putStrLn "Hello, what is you r name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", how are you?")
