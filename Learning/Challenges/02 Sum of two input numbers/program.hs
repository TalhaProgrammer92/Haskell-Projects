{-
Write a program to take two numbers and display their sum as:
Sum of these numbers is: <sum>
-}

main :: IO()
main = do
    -- Get the first number
    putStrLn "Enter first number:"
    num1 <- getLine

    -- Get the second number
    putStrLn "Enter second number:"
    num2 <- getLine

    -- Calculate the sum
    let sum = (read num1 :: Int) + (read num2 :: Int)

    -- Display the sum
    putStrLn ("Sum of these numbers is: " ++ show sum)
