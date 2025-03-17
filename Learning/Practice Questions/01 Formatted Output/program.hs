{-
Write a program that take name and age as input and display the following output:
Hello [name], you are [age] years old!
-}

main :: IO ()
main = do
    -- Get the name and age from the user
    putStrLn "Enter your name:"
    name <- getLine
    putStrLn "Enter your age:"
    age <- getLine

    -- Display the formatted output
    putStrLn $ "Hello " ++ name ++ ", you are " ++ age ++ " years old!"