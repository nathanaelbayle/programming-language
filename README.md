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


![picture alt](http://via.placeholder.com/200x150 )













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
















