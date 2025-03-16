### 🚀 **One-Day Haskell Learning Plan** (From Zero to Functional Hero)  

This plan assumes **intense focus for one day (~6–8 hours total)**. If you follow this, you'll cover **all** the fundamentals **fast**.  

---

## 🛠 **Step 1: Setting Up Haskell in VS Code** (30 min)  
Before we jump into coding, let’s set up **VS Code** to write and run Haskell programs.  

### 1️⃣ **Install GHC (Glasgow Haskell Compiler) & Haskell Toolchain**  
Download and install **GHCup** (which manages Haskell compilers and tools):  
🔗 **[Download GHCup](https://www.haskell.org/ghcup/)**  

During installation, select:  
✅ GHC (Haskell Compiler)  
✅ Cabal (Build system)  
✅ HLS (Haskell Language Server)  

After installation, open **Command Prompt (cmd)** and verify:  
```sh
ghc --version
cabal --version
haskell-language-server --version
```
If these show versions, setup is **successful**.  

### 2️⃣ **Install Haskell Extension in VS Code**  
- Open **VS Code**  
- Go to **Extensions (Ctrl + Shift + X)**  
- Search for **"Haskell"** and install **Haskell extension**  
- Restart VS Code  

### 3️⃣ **Run Haskell in VS Code**  
1. Create a **new folder** for Haskell projects  
2. Open VS Code in that folder  
3. Create a file `main.hs` and write:  
   ```haskell
   main :: IO ()
   main = putStrLn "Hello, Haskell!"
   ```  
4. Open a terminal (**Ctrl + `**) and run:  
   ```sh
   runhaskell main.hs
   ```
   It should print:  
   ```
   Hello, Haskell!
   ```

✅ **Setup complete!** 🎉 Now let's learn Haskell!  

---

# 🏆 **One-Day Haskell Learning Plan**  

## ⏳ **Hour 1: Basic I/O, Variables, and Data Types**  
🔹 **Concepts**:  
- Printing (`putStrLn`, `putStr`)  
- Taking input (`getLine`, `read`)  
- Variables (`let`, `where`)  
- Data types (`Int`, `Float`, `Bool`, `String`, `Char`, `List`, `Tuple`)  

📝 **Example**:  
```haskell
main :: IO ()
main = do
    putStr "Enter your age: "
    age <- getLine
    let ageNum = read age :: Int
    putStrLn ("In 10 years, you'll be " ++ show (ageNum + 10))
```
🎯 **Goal**: Understand **type conversion (`read`, `show`)** and **basic I/O**.

---

## ⏳ **Hour 2: Conditionals & Pattern Matching**  
🔹 **Concepts**:  
- `if-then-else`  
- Guards (`|`)  
- Pattern Matching (`case`, function patterns)  

📝 **Example**:  
```haskell
checkNum :: Int -> String
checkNum n
    | n < 0     = "Negative"
    | n == 0    = "Zero"
    | otherwise = "Positive"

main :: IO ()
main = do
    putStr "Enter a number: "
    num <- getLine
    let n = read num :: Int
    putStrLn ("This number is " ++ checkNum n)
```
🎯 **Goal**: Learn **decision making** in Haskell.

---

## ⏳ **Hour 3: Functions & Recursion**  
🔹 **Concepts**:  
- Function syntax  
- Recursion (since loops don't exist in Haskell!)  

📝 **Example** (Recursive factorial):  
```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    putStr "Enter a number: "
    num <- getLine
    let n = read num :: Int
    putStrLn ("Factorial is " ++ show (factorial n))
```
🎯 **Goal**: Understand how Haskell replaces **loops** with **recursion**.

---

## ⏳ **Hour 4: Lists & List Functions**  
🔹 **Concepts**:  
- Lists (`[1,2,3]`, `head`, `tail`, `length`)  
- List Comprehension  
- `map`, `filter`, `foldr`, `foldl`  

📝 **Example**:  
```haskell
main :: IO ()
main = do
    let numbers = [1,2,3,4,5]
    let squared = map (^2) numbers
    let evens = filter even numbers
    putStrLn ("Squared: " ++ show squared)
    putStrLn ("Evens: " ++ show evens)
```
🎯 **Goal**: Get **comfortable** with **lists** and **higher-order functions**.

---

## ⏳ **Hour 5: Tuples & Type Classes**  
🔹 **Concepts**:  
- Tuples (`(1, "Hello")`)  
- Custom Types (`type`, `data`)  
- Type Classes (`Num`, `Eq`, `Show`)  

📝 **Example** (Custom Data Type):  
```haskell
data Person = Person String Int  -- Name, Age

describePerson :: Person -> String
describePerson (Person name age) = name ++ " is " ++ show age ++ " years old."

main :: IO ()
main = do
    let p1 = Person "Alice" 25
    putStrLn (describePerson p1)
```
🎯 **Goal**: Learn how Haskell **structures data**.

---

## ⏳ **Hour 6: Lambda Functions & High-Order Functions**  
🔹 **Concepts**:  
- Lambda (`\x -> x + 1`)  
- Using `map`, `filter`, `foldr`  

📝 **Example**:  
```haskell
main :: IO ()
main = do
    let numbers = [1,2,3,4,5]
    let squared = map (\x -> x * x) numbers
    putStrLn ("Squared: " ++ show squared)
```
🎯 **Goal**: Get **comfortable** with **anonymous functions**.

---

## ⏳ **Hour 7: List Comprehension & Infinite Lists**  
🔹 **Concepts**:  
- Generating lists  
- Lazy evaluation  

📝 **Example**:  
```haskell
evens = [x | x <- [1..20], even x]
main = print evens
```
🎯 **Goal**: Learn **list generation tricks**.

---

## 🎯 **Final Hour: Mini Project** (Tic-Tac-Toe or Calculator)  
Use all the concepts in a **real program**.  

---

# 🎉 **By the End of the Day...**  
- **You can write** real Haskell programs.  
- **You understand** recursion, lists, pattern matching, and type systems.  
- **You’re ahead** of 95% of people who quit Haskell in the first hour. 😂  

---

### 🤖 **Next Steps?**  
- **Day 2:** Learn **Monads & IO Monad** (Real functional programming power).  
- **Day 3:** Build **a small project** in Haskell (maybe even **Tic-Tac-Toe**).