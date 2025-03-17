# Learning Roadmap of Haskell

- - -

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

- - -

# Practice Problems of Haskell

- - -

You're right! Here’s a **structured roadmap** with **practice questions** for each concept to **master Haskell step by step.** 🚀🔥  

---

## **🚀 Haskell Learning Roadmap with Practice Questions**
> **Goal:** Learn Haskell fundamentals in **one day** with hands-on coding.  

### **📌 Step 1: Basic I/O & Variables**
#### **Concepts:**
- `putStrLn` → Print a string with newline  
- `putStr` → Print without newline  
- `getLine` → Take user input (returns a string)  
- `read` & `show` → Convert between string & other types  

#### **Practice Questions:**  
1️⃣ Write a program that asks for the user's name and age, then prints:  
   `"Hello [name], you are [age] years old!"`  

2️⃣ Modify your program to ask for two numbers and print their **sum**.  

---

### **📌 Step 2: Conditionals (`if-else`, Guards)**
#### **Concepts:**
- `if-else` syntax  
- Guards (`| condition = result`)  
- `otherwise` as the default case  

#### **Practice Questions:**  
3️⃣ Write a program that asks for a number and prints **"Even"** or **"Odd"**.  

4️⃣ Write a function `maxOfThree a b c` that returns the largest of three numbers.  

---

### **📌 Step 3: Functions & Recursion**
#### **Concepts:**
- Defining functions  
- Recursion (Haskell doesn’t have loops like other languages)  
- Pattern matching  

#### **Practice Questions:**  
5️⃣ Write a recursive function `factorial n` to compute `n!`.  

6️⃣ Write a recursive function `fib n` to return the `n`th Fibonacci number.  

---

### **📌 Step 4: Lists & List Operations**
#### **Concepts:**
- Creating lists (`[1, 2, 3, 4]`, `[1..10]`)  
- List functions (`length`, `head`, `tail`, `reverse`)  
- List comprehensions  

#### **Practice Questions:**  
7️⃣ Write a function `sumList lst` that returns the sum of all elements in a list.  

8️⃣ Create a list comprehension that returns all **even** numbers from 1 to 50.  

---

### **📌 Step 5: Higher-Order Functions (`map`, `filter`, `foldl`)**
#### **Concepts:**
- `map` → Apply a function to every list element  
- `filter` → Keep only elements that satisfy a condition  
- `foldl` → Reduce a list to a single value  

#### **Practice Questions:**  
9️⃣ Write a function `squareList lst` that returns a new list with each number squared.  

🔟 Write a function `sumEven lst` that sums all **even numbers** in a list using `filter`.  

---

### **📌 Step 6: Custom Data Types & Pattern Matching**
#### **Concepts:**
- Creating `data` types  
- Pattern matching with `case`  
- Using `deriving Show`  

#### **Practice Questions:**  
1️⃣1️⃣ Define a data type `Shape` with constructors for `Circle r` and `Rectangle w h`.  
Write a function `area` that calculates the area of a shape.  

1️⃣2️⃣ Define a `Person` type with `name` and `age`. Write a function `isAdult` that returns `True` if age ≥ 18.  

---

### **📌 Step 7: Lazy Evaluation & Infinite Lists**
#### **Concepts:**
- Lazy evaluation  
- Infinite lists (`[1..]`, `cycle`, `repeat`, `iterate`)  

#### **Practice Questions:**  
1️⃣3️⃣ Create an infinite list of all Fibonacci numbers and print the first 10 elements.  

1️⃣4️⃣ Use `take` to get the first 10 even numbers from an infinite list.  

---

### **📌 Step 8: Modules & Importing Libraries**
#### **Concepts:**
- Importing modules  
- Using `Data.List` for advanced list functions  
- Using `System.IO` for file handling  

#### **Practice Questions:**  
1️⃣5️⃣ Write a program to read a file, reverse its content, and write to a new file.  

1️⃣6️⃣ Use the `sort` function from `Data.List` to sort a list of numbers.  

---

## **🔥 Final Challenge: Build a Small Project**
1️⃣7️⃣ **FizzBuzz Generator**: Modify the `fizzBuzz` function to work for an **infinite** list and print first 30 results.  

1️⃣8️⃣ **Mini Calculator**: Write a simple REPL (Read-Eval-Print Loop) where users enter a math expression (`"5 + 3"`) and get the result.  

---

### **⏳ How to Follow This Plan?**
⏳ Spend **15-30 min per step** and **code each solution**. In **one day**, you’ll have solid **Haskell fundamentals**! 🚀  