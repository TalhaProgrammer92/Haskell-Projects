{-
Write a program that take two numbers and display their sum as:
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
    let sum = (read num1 :: Int) + (read num2 :: Int)   -- read is used to convert string to other (integer)

    -- Display the sum
    putStrLn ("Sum of these numbers is: " ++ show sum)  -- show is used to convert other (integer) to string
