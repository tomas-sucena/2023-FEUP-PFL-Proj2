# PFL - Pretty Fun Language

Welcome to the official documentation of **Pretty Fun Language** (or PFL for short), which is a small imperative programming language developed in Haskell.

**Class:** 3LEIC11   
**Group:** 4

| **Student ID** |   **Student Name**   | **Participation** |
|:--------------:|:--------------------:|:-----------------:|
|   202108691    | João Afonso Viveiros |        50%        |
|   202108701    |  Tomás Sucena Lopes  |        50%        |

## Table of Contents
<!-- TOC -->
* [PFL - Pretty Fun Language](#pfl---pretty-fun-language)
  * [Table of Contents](#table-of-contents)
  * [Development](#development)
    * [Lexical analysis](#lexical-analysis)
    * [Parsing](#parsing)
    * [Compilation](#compilation)
<!-- TOC -->

## Development

PFL is a hybrid programming language, meaning that it is simultaneously **compiled** and **interpreted**. In fact, its code is converted into a series of intermediate instructions, known as bytecode, that are then executed by PFL's virtual machine.
As such, running PFL is a four-phase process:

1. **Lexical analysis -** The source code is divided into **tokens**, which are groups of characters that have a collective meaning. Examples of tokens include variables, keywords and literals.
2. **Parsing -** The list of tokens is analyzed to form syntax trees, which are the data structures that confer meaning to the ...
3. **Compilation -** The statements and expressions are compiled into PFL's bytecode.
4. **Execution -** The bytecode is interpreted by the virtual machine, thus producing the actual output of the code.

### Lexical analysis

The first phase of the compilation process is **lexing**, which consists in reading the code input by the programmer and dividing it into semantic atoms - **tokens**.

Given there are several types of tokens, we created the following data structure to more easily represent them:

```haskell
data Token =
  Var String    -- variables

  -- values
  | I Integer   -- integers
  | B Bool      -- booleans

  -- symbols
  | Semicolon   -- ';'
  | LParen      -- '('
  | RParen      -- ')'

  -- operators
  | Assign      -- ':='
  | Add         -- '+'
  | Mult        -- '*'
  | Sub         -- '-'
  | Le          -- '<='
  | IEqu        -- '=='
  | Not         -- 'not'
  | BEqu        -- '='
  | And         -- 'and'
  | Or          -- 'or'

  -- keywords
  | If          -- 'if'
  | Then        -- 'then'
  | Else        -- 'else'
  | While       -- 'while'
  | Until       -- 'until'
  | Do          -- 'do'

  deriving Show
```

Due to Haskell's pattern matching, lexing the source code is as simple as examining the current character and stepping into the function that creates the adequate token.
Below are a few examples:

* If the character is a digit, we are lexing an **integer**. So, we sequentially analyze the next characters until the current character is no longer a digit, at which point we convert the substring into the corresponding integer value.

```haskell
lexDecNumber :: String -> (Token, String)
lexDecNumber s = (Token.I (read number :: Integer), s')
  where
    (number, s') = break (not . isDigit) s

...

lexer :: String -> [Token]
lexer s@(c:_)
  | isDigit c = token : lexer s'
  where (token, s') = lexDecNumber s
```

* If the character is a lowercase letter, we are either lexing a **variable** or a reserved **keyword** (like _if_, _else_, etc.). To find out which of those it is, we sequentially read the next characters until the current character is neither a letter nor a digit. Then, we compare the substring with the language keywords to see if it is one.

```haskell
lexWord :: String -> (Token, String)
lexWord s = (token, s')
  where
    (word, s') = break (not . isAlphaNum) s
    token = case word of
      "not" -> Token.Not
      "and" -> Token.And
      "or" -> Token.Or
      "if" -> Token.If
      "then" -> Token.Then
      "else" -> Token.Else
      "while" -> Token.While
      "until" -> Token.Until
      "do" -> Token.Do
      "True" -> Token.B True
      "False" -> Token.B False
      _ -> Token.Var word
      
...

lexer :: String -> [Token]
lexer s@(c:_)
  | isAlpha c = token : lexer s'
  where (token, s') = lexWord s
```

**Note:** In PFL, variable names can contain digits, but they cannot start with one. For example, _var1_ is a valid variable name, whereas _1var_ is not.

* If the character is a symbol, then it could either be an **operator** or a language **symbol** (parenthesis or semicolon).

```haskell
lexer :: String -> [Token]
-- symbols
lexer (';':s) = Token.Semicolon:(lexer s)
lexer ('(':s) = Token.LParen:(lexer s)
lexer (')':s) = Token.RParen:(lexer s)

-- operators
lexer (':':'=':s) = Token.Assign:(lexer s)
lexer ('<':'=':s) = Token.Le:(lexer s)
lexer ('=':'=':s) = Token.IEqu:(lexer s)
lexer ('+':s) = Token.Add:(lexer s)
lexer ('*':s) = Token.Mult:(lexer s)
lexer ('-':s) = Token.Sub:(lexer s)
lexer ('=':s) = Token.BEqu:(lexer s)
```

The source code of the lexer can be found [here](src/Lexer.hs).

### Parsing

### Compilation

Upon obtaining the ASTs, the next step is converting them into proper instructions, which will then be executed in the final phase. This phase is where the **compilation** itself takes place.

Instead of targetting specific hardware i.e. compiling to architecture dependent instructions, we opted to compile to intermediate instructions - **bytecode** - which are then interpreted by PFL's **virtual machine**. This approach ensures PFL is a portable language, albeit at the cost of performance.

The instructions supported by PFL's virtual machine are as follows:

```haskell
data Inst =
  Push Integer          -- push an integer value to the stack
  | Add                 -- add two integers and push the result to the stack
  | Mult                -- multiply two integers and push the result to the stack
  | Sub                 -- subtract two integers and push the result to the stack
  | Tru                 -- push 'True' to the stack
  | Fals                -- push 'False' to the stack
  | Equ                 -- test if two values are equal and push the result to the stack
  | Le                  -- test if an integer is less or equal than another and push the result to the stack
  | And                 -- perform a logical AND between two booleans and push the result to the stack
  | Neg                 -- negate a boolean and push the result to the stack
  | Fetch String        -- fetch the value of a variable and push it to the stack
  | Store String        -- pop the value on top of the stack and store it in a variable
  | Noop                -- do nothing
  | Branch Code Code    -- conditional branch
  | Loop Code Code      -- loop
  deriving Show
```

Similarly to the ASTs, we divided the compilation into three sub-processes: compiling **arithmetic expressions**, compiling **boolean expressions** and compiling **statements**. Despite this, 

### Execution

