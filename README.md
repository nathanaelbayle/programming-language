# **WORK IN PROGRESS** 

# Programming Language

## #include glossary


## Objectives
 The course gives a practical and theoretical introduction to semantics of programming languages and to a range of important constructs and abstractions used in contemporary languages. The (indirect) goal is that an increased understanding of programming languages translates to higher quality of programs that students write and higher productivity in their programming. The class also equips the student with basic skills of designing and implementing small languages, where an implementation consists of a parser, type checker, and an interpreter.

## Content
The course covers important language constructs in modern languages. It discusses the specification of the syntax and semantics of programming languages, and introduces the notions of a type system and type safety. Students implement a series of interpreters and type checkers. Reflecting on the influence of the properties of a programming language and its features on software quality is a central theme of the course.

## Learning Outcomes

### Knowledge

- explain the concepts of concrete and abstract syntax of a language, and the concerns of designing syntax that can be parsed effectively; and select parsing tools and approaches according to problem context.
- explain notations and approaches to defining semantics of programming languages, in particular those of operational semantics.
- explain the impacts of evaluation order to termination, to efficiency, and the ease or difficulty of reasoning about programs.
- explain what type safety of a programming language means.
- explain different variations of polymorphism, and relate them to features of contemporary programming languages.
- explain the essence of important programming language constructs and concepts, describe their implementation approaches, purpose and productive use in programming, and their manifestations and variations in different mainstream programming languages.

### Skills

- define an abstract grammar for a small programming language and implement a parser for it.
- define an operational semantics for a small language and implement it as an interpreter.
- define and implement a type system for a small language.

### General competence

- make justified decisions about the use of different programming language constructs in programming.
- make justified decisions about selecting programming languages for software projects.
- follow new developments in programming languages.
- read and understand, to a useful degree, scholarly articles in the area of programming languages.


## About concrete and abstract syntax

*Concrete syntax* is concerned with the precise syntax of a program construct, whereas *abstract syntax* with those components of the construct that are essential for its semantics. 
For example, consider the (Haskell) expression:
```haskell
if a == 0 then "yes" else "no"
````

The concrete syntax is worried about keywords sych as `if`, `then`, `else`, names of operations `==`, and maybe even the layout of the text and comments.

An *abstract syntax tree* (AST) is a program representation free of such clutter. They are intended for defining the semantics and runtime properties only, not for editing or refactoring of source code. ASTs are defined by *abstract grammars*, which can be ambiguous, as they are not used for defining parsers. Abstract grammars can be defined by grammar formalisms, signature declarations, as algebraic data types, or as class hierarchies in an object-oriented (OO) setting.

A possible AST of the above example Haskell expression is as follows. It contains all the nodes that are necessary for further analysis of the program fragment.

![picture alt](https://github.com/nathanaelbayle/programming-language/blob/main/pic_1.PNG)

The *if expression* has three components, the condition and the value of the two branches.
The *comparison expression* (the `==` expression) has two integer subexpressions.

## Specifying abstract syntax

An *abstract syntax tree* (AST) is a node-labeled *multiway tree* (popularly celled *rose tree* by functional programmers). In a rose tree the number of child nodes for each node can be arbitrary. The labels indicate what kind of a language construct the node represents (such as a variable, function call, or integer constant, to give a few examples). The labels typically translate to node types in an AST implementation.

### Extended Signature Noration (ESL)

The notations for specifying ASTs (both graphical and textual) vary; here we use Ralf Lämmel’s *Extended Signature Notation* (ESL). In this notation, ASTs are specified using *algebraic signatures*. They consist of a set of sorts and typed function symbols. Sorts appear as argument and parameter types of the functions. In common notations, signatures list sorts explicitly, but here they are implicit: the set of sorts are the sorts appearing in at least one function type.

> Although seemingly simpler for small examples, such a lack of declarations is known to delay discovery of simple misprints, and may also be counterproductive when it comes to analysing a language by not forcing clear reasoning at an early stage.

### Standard presentation of BTL in ESL

Let us take a simple example, the *Basic TAPL Language* (BTL). This is a very simple language consisting of two kinds of values, booleans and natural numbers, and incrementing and decrementing operations, a test for zero, and a conditional expression. BTL has no variables.

Here is a simple BTL program:
``` haskell
pred ( if ( iszero zero ) then succ ( succ zero ) else zero )
```

The BTL specification in ESL is as follows:
````haskell
symbol true: -> expr; -- The Boolean "true"
symbol false: -> expr; -- The Boolean "false"
symbol zero: -> expr; -- The natural number zero
symbol succ: expr -> expr; -- Successor of a natural number
symbol pred: expr -> expr; -- Predecessor of a natural number
symbol iszero: expr -> expr; -- Test for a number to be zero
symbol if: expr × expr × expr -> expr; -- Conditional
````
Here we do not separate integer expressions from boolean expressions.

Thus the following unintended expression is valid according to the syntax.
``` haskell
pred ( if zero then succ ( succ true ) else zero )
```

### Fully typed presentation of BTL in ESL

The example below shows a few typed function symbols as part of an ESL signature. The sorts here are `Bexpr` (boolean expression), `Iexpr` (integer expression). Here we provide two choice statements, one for booleans and one for integers, and some operations on each of the basic types.

``` haskell
symbol true : -> Bexpr; -- The Boolean "true"
symbol false : -> Bexpr; -- The Boolean "false"
symbol zero : -> Iexpr ; -- The natural number zero
symbol succ : Iexpr -> Iexpr ; -- Successor of a natural number
symbol pred: Iexpr -> Iexpr ; -- Predecessor of a natural number
symbol iszero : Iexpr -> Bexpr; -- Test for a number to be zero
symbol boolIf : Bexpr × Bexpr × Bexpr -> Bexpr; -- choice between booleans
symbol intIf : Bexpr × Iexpr × Iexpr -> Iexpr ; -- choice between integers
```

The set of ASTs (programs) is defined as all the *terms* that can be *generated* from the signature. Generating means listing all type-correct compositions of the functions. For example, we could construct the AST for the above expression as follows.
``` haskell
pred (
 intIf ( iszero ( zero () ) , succ ( succ zero () ) , zero () )
 )
```

The following non-term is an example of a non-term in this presentation of BTL, though it was valid in the previous AST description. It is invalid because `zero()` is an `Iexpr` not a `Bexpr` as required by the `if` expressions, `true()` is an inappropriate argument for `succ` , `succ(..)` and `zero()` are inappropriate arguments for `boolIf`, and the return type of `boolIf` is not compatible with `Iexpr` as required by `pred`.
``` haskell
pred (
 boolIf ( zero () , succ ( succ true () ) , zero () )
 )
 ```
 
Being this precise with the type of each subterm for each expression comes at a price: a large AST signature.


### Less stringent presentation of BTL in ESL

We can relax the precision of the AST by using a more general `expr` for all expressions, and introduce a general function call with a function name rather than declaring each specific function. According to convention we keep `if` as a special language feature. The `*` symbol indicates zero or more occurrences of a sort.
```haskell
symbol if : expr × expr × expr -> expr;
symbol functionCall : string × expr∗ -> expr;
```

This lets us construct terms for most expressions without adding more symbols to the signature. For example, we can construct the AST for the above expression as follows. 
```haskell
functionCall ( "pred",
  if ( functionCall (" iszero ", functionCall ("zero")) ,
    functionCall ("succ", functionCall ("succ", functionCall ("zero"))) ,
    functionCall ("zero")
  )
)
```

The following non-term is an example of an invalid composition. It is invalid because the number of arguments to `if` is wrong.
```haskell
if ( functionCall ("zero") , functionCall (" iszero ", functionCall ("true")) )
```

Note that the non-term examples of the previous subsections are terms of the current signature (after the relevant rewrites).
```haskell
functionCall ( "pred",
  if ( functionCall ("zero") ,
    functionCall ("succ", functionCall ("succ", functionCall ("true"))) ,
    functionCall ("zero")
  )
)
```

We cannot capture that the language specification requires that the first parameter of `if` must be an expression that evaluates to a boolean value, the argument to `succ` must be an integer expression, and so forth. Such violations must be caught later, by the type-checker, or at run time.










## Introduction

## Abstract Syntax
There is two forms of syntax
- Concrete syntax : what the programmer writes
- Abstract syntax : what is essential for the semantics

Absttract syntax originally developed for compiling
- Ignore comments
- Ignore layout
- Comments important for documentation (Javadoc, Doxygene, ... )
- Comments and layout important for IDEs

#### Basil Signature Language (BSL)
- Functions types:
  - Function symbol
  - list of argument sorts
  - result sort
- Sorts implicit 

#### Extended Signature Language (ESL)
- BSL Symbol declarations
- Type declarations (aliases) list types t*, t
- Optional types t?
- Tuple types t1 × ... × tn
- Primitive types
  - boolean, 
  - integer, 
  - float, 
  - string, 
  - term


## Extended BTL abstract syntax

**Symbol** uplus : expr → expr ; \
**Symbol** uminus : expr → expr ;\
**Symbol** unot : expr → expr ;\
**Symbol** intLiteral : integer → expr ;\
**Symbol** booLiteral : boolean → expr ;\
**Symbol** varid : string → expr ;\
**Symbol** functionCall : string × expr → expr ;\
**Symbol** binaryExpr : binop × expr × expr → expr ; \
**Symbol** ifexpr : expr × expr × expr → expr ; 

**Symbol** plus : → binop ;\
**Symbol** minus : → binop ;\
**Symbol** or : → binop ;\
**Symbol** mult : → binop ;\
... \
**Symbol** le : → binop ;\
**Symbol** ge : → binop ;



## Represent Object language in Metalanguage
The ESL difines the abstract syntax of an Object Language. \
We will use a Metalanguage for our tools :
- How to represent abstract syntax in the metalanguage
- Use algebraic data types (Haskell)
  - Each ESL sort becomes a new data type
  - Each ESL function becomes a case for the result type
  - Extending the object language requires adding cases
- Object-oriented languages: Use class hierarchy
  - Each ESL sort becomes a root class
  - Each ESL function becomes a subclass (constructor)
  - Allows independent object language extensions

## Language investigation framework

Object language: the language of study
- Programming language concepts
  - BTL: Expressions
  - BIPL: Expressions + statements
  - Pascal: structured programming language
  - ADT-Pascal: generic programming language
Metalanguage: the language of the tools
- Haskell: a lazy functional language
  - Represent object language abstract syntax
  - Object language analysis
  - Object language transformation
  - Object language evaluation


## Type analysis

### Type Checking
- Checks that a program is well typed
- Discovers many subtle programming errors
### Type inference
- Takes an AST and creates a new, typed AST
  - Typed AST: Every node is decorated with its type
### Two main approaches

#### Bottom-up typing
- Variables and function declarations are known : infers type of expressions
#### Top-down typing
- Type declarations and algorithms are known infers most general consistent type for all code

## Dynamic semantics

### Evaluator for expressions (BTL)


## Value Domain
Common computer integers 
- 8, 16, 32, 64 bit two’s-complement (Haskell: Int)
- 8, 16, 32, 64 bit unsigned (Haskell: Word)

Common computer reals
- IEEE754: 32, 64, 80 bit floating point

Common computer character strings (replaces Pascal char)
- Unicode: UTF-8, UTF-16, UTF-32

What about other Pascal types?
- Booleans: 0, 1 encoded in Word8
- Enumerations (scalar types): Word8 for ≤256 values
- Set type: 256 bits (4*Word64)
- Pointers: locations in memory
- Array types
- Record types




### Interpreters for statements (BIPL)

#### Basic Imerative Programming Language
















