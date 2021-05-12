# Programming Language

### Introduction

### Abstract Syntax
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
- Type declarations (aliases) list types t*, t+
- Optional types t?
- Tuple types t1 × ... × tn
- Primitive types
  - boolean, 
  - integer, 
  - float, 
  - string, 
  - term


### Extended BTL abstract syntax

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



### Represent Object language in Metalanguage
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




















