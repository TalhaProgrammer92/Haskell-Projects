-- Program to take age input and convert it to integer

-- main function
main :: IO ()
main = do
    -- get age input
    putStrLn "Enter your age: "
    age <- getLine

    -- convert age to integer
    let ageInt = read age :: Int

    -- print age in 10 years
    putStrLn ("In 10 years you will be " ++ show (ageInt + 10))