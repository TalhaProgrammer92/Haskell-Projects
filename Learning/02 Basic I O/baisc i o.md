# Basic I/O Program in Haskell
```hs
main :: IO ()
main = do
    -- get age input
    putStrLn "Enter your age: "
    age <- getLine

    -- convert age to integer
    let ageInt = read age :: Int

    -- print age in 10 years
    putStrLn ("In 10 years you will be " ++ show (ageInt + 10))
```

- - -

# Program Breakdown

### 1. **`putStrLn "Enter your age: "`**
   - This line prints the message `"Enter your age: "` to the console.
   - `putStrLn` is an I/O function that takes a string and prints it with a newline.

### 2. **`age <- getLine`**
   - `getLine` is an I/O action that reads a line of input from the user.
   - The `<-` operator extracts the input (which is a `String`) and binds it to the variable `age`.

### 3. **`let ageInt = read age :: Int`**
   - `read age` converts the string input into an integer.
   - `:: Int` explicitly tells Haskell to interpret the value as an `Int`.

### 4. **`putStrLn ("In 10 years you will be " ++ show (ageInt + 10))`**
   - `ageInt + 10` calculates the user's age in 10 years.
   - `show (ageInt + 10)` converts the integer back into a string.
   - The `++` operator concatenates the strings to form the final message.
   - `putStrLn` then prints the result to the console.

### **Summary**
This program prompts the user for their age, converts the input into an integer, adds 10 to it, and prints the result.