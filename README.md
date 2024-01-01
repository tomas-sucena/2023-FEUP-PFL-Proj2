# PFL - Pretty Fun Language

Welcome to the official documentation of **Pretty Fun Language** (or PFL for short), which is a small imperative programming language developed in Haskell.

**Class:** 3LEIC11   
**Group:** 4

| **Student ID** | **Student Name**     | **Participation** |
|:--------------:|:--------------------:|:-----------------:|
|   202108691    | João Afonso Viveiros |        50%        |
|   202108701    |  Tomás Sucena Lopes  |        50%        |

## Table of Contents
<!-- TOC -->
* [PFL - Pretty Fun Language](#pfl---pretty-fun-language)
  * [Table of Contents](#table-of-contents)
  * [Development](#development)
<!-- TOC -->

## Development

PFL is a hybrid programming language, meaning that it is simultaneously **compiled** and **interpreted**. In fact, its code is converted into a series of intermediate instructions, known as bytecode, that are then executed by PFL's virtual machine.
As such, running PFL is a four-phase process:

1. **Lexical analysis -** The source code is divided into **tokens**, which are groups of characters that have a collective meaning. Examples of tokens include variables, keywords and literals.
2. **Parsing -** The list of tokens is analyzed to form syntax trees, which are the data structures that confer meaning to the ...
3. **Compilation -** The statements and expressions are compiled into PFL's bytecode.
4. **Execution -** The bytecode is interpreted by the virtual machine, thus producing the actual output of the code.

### Lexing

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

lexer s@(c:_)
  | isDigit c = token : lexer s'
  where (token, s') = lexDecNumber s
```

