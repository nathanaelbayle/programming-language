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

&nbsp;
&nbsp;

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
&nbsp;
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

### Universal representation of ASTs in ESL

It is also possible to use very general data type in the metalanguage that can represent any signature—the same data type could be used for ASTs of any object language.
```haskell
symbol term: constructorid × termrep∗ -> termrep;
type constructorid = string;
```

This presentation is so general that we basically have lost all specifics of BTL. Any BTL term can be represented using this signature, as well as many terms that do not represent meaningful BTL programs.

The following are all valid  `TermRep` ASTs. The first is intended to be a valid BTL term, the latter two fail to be BTL terms, as explained in the previous sections.
```haskell
Term ("pred",
    Term("if", Term(" iszero ",Term("zero")) ,
    Term("succ",Term("succ",Term("zero"))) ,
    Term("zero")
  ))
Term ("pred",
    Term("if", Term("zero") ,
    Term("succ",Term("succ",Term("true"))) ,
    Term("zero")
  ))
Term("if", Term("zero") ,
    Term(" iszero ",Term("true"))
)
```

We would need to apply static analysers, to check if such an AST actually represents BTL code (or possibly something else).

Such a *universal* representation can be useful in various metaprogramming applications, enabling algorithms that can operate on *all* data types (think of things like serialization, traversals through data types, etc.).
&nbsp;

## Representing ASTs in a meta-language

The declaration of AST signatures defines (a superset) of valid terms of the *object language*, the programming language, e.g., BIPL, we want to study. Now we want to represent the object language AST in our *metalanguage*, the programming language, e.g., Haskell, we use for implementing our tools.

There is a wide array of choices on how to represent programs in the object language with a chosen metalanguage. If the metalanguage is typed, the types can be taken advantage of to making only valid ASTs to be representable, or at least to make some invalid ASTs unrepresentable. If a typed functional language is used as a metalanguage, algebraic data types provide a rather straightforward encoding of *algebraic signatures* (and of ESL). In an untyped functional language, terms are constructed with lists: they capture the tree structure, but do not enforce type-correct composition of terms. Object-oriented languages as metalanguages typically map sorts to classes and represent subterms as member variables. Typing of member variables rules out type incorrect ASTs


### Standard presentation of BTL in Haskell

The question is how to represent terms generated from a signature. Haskell’s data types are rather well-suited for representing ASTs, since they are a fairly direct representation of algebraic signatures. Below, we define a data type for BTL. `TRUE` and `FALSE` are in all-caps because the names `True` and `False` are taken by the standard library.
```haskell
data Expr
= ETrue
| EFalse
| EZero
| ESucc Expr
| EPred Expr
| EIsZero Expr
| EIf Expr Expr Expr
```

With this representation, the `sampleExpr` from above is written as:
```haskell
EPred
  (EIf
    (EIsZero
      EZero)
    (ESucc
   (ESucc
     EZero))
  EZero)
```

### Opaque presentation of BTL in Haskell

Above, the meta-programmer has full access to the AST representation. One can imagine scenarios where the representation ought to be kept hidden, and access to the AST, maybe just parts of it, should be via an abstract data type. A brief outline of one such possible Haskell representation is as follows. Here, each function constructs ASTs of some opaque type `ExpQ`, whose definition is not accessible.
```haskell
-- Abstract data type for Btl expressions (object language) in Haskell (metalanguage)
trueE :: () −> ExpQ
falseE :: () −> ExpQ
zeroE :: () −> ExpQ
succE :: ExpQ −> ExpQ
predE :: ExpQ −> ExpQ
iszeroE :: ExpQ −> ExpQ
ifE :: ExpQ −> ExpQ −> ExpQ −> ExpQ
```

### Standard presentation of BTL using OO

Finally, for balance, here’s a possible representation in an object-oriented meta-language (C++). The expr sort is represented with an abstract base class from which each different kind of expression derives from.
```haskell
struct Expr { virtual ~Expr() {}; };
struct True : Expr {};
struct False : Expr {};
struct Zero : Expr {};
struct Succ : Expr {
  Expr∗ e ;
  Succ(Expr∗ e) : e(e) {}
};
struct Pred : Expr {
  Expr∗ e ;
  Pred(Expr e) : e(e) {}
};
struct IsZero : Expr {
  Expr∗ e ;
  IsZero (Expr∗ e) : e(e) {}
};
struct If : Expr {
  Expr∗ e0 , e1, e2 ;
  If (Expr∗ e0 , Expr∗ e1 , Expr∗ e2)
    : e0(e0) , e1(e1) , e2(e2) {}
};
```

Instead of raw pointers, it would be advisable to use shared pointers (`std::shared_ptr`), or have the structs assume ownership of their child nodes and therein take responsibility for their (de)allocation. The `Expr` node could store data relevant for all nodes (such as the source location of the concrete syntax that gave rise to the node). The structs could support a *visitor* interface for helping in extending the language and its functionality. In general, there are several design choices in how to best represent abstract syntax trees.

### Haskell representation of universal presentation

We can define ASTs for the universal representation,
```haskell
-- Representation of any object language in Haskell (metalanguage)
data TermRep = Term ConstrId [TermRep]
type ConstrId = String
```

For example, the object language term `pred ( if ( iszero zero ) ( succ ( succ zero )) zero )` would be representated as follows in Haskell using
```haskell
termExample1 = Term "pred"
  [Term "if" [Term " iszero " [Term "zero" []],
    Term "succ" [Term "succ" [Term "zero" []]],
    Term "zero" []
  ]]
```

The two bad terms from the discussion in Section 2 will be as follows.
```haskell
termExample2 :: TermRep
termExample2 = Term "pred"
  [Term "if" [Term "zero" [], Term "succ" [Term "succ" [Term "true" []]], Term "zero" []]]
termExample3 :: TermRep
termExample3 = Term "if" [Term "zero" [Term " iszero " [Term "true" []]]]
```

## Further Examples

### Standard presentation of BIPL in ESL
Lämmel’s *Basic Imperative Programming Language* (BIPL) as an example for a full AST specification. BIPL’s constructs are assignments, sequences, conditionals, and loops. The following example demonstrates its concrete syntax.
```c++
{
  // sample operands for euclidian division of x by y
  x = 14; y = 4;
  // compute quotient q=3 and remainder r=2
  q = 0;
  r = x;
  while (r >= y) {
    r = r − y;
    q = q + 1;
  }
}
```

BIPL’s abstract syntax is shown below. The function signatures that have no parameters define constants. This signature separates statments from expressions (but no more detailed typing), but keeps track of the exact number of arguments to a function (and thus has to keep track of all functions).
```haskell
-- Statements
symbol skip : -> stmt;
symbol assign : string × expr -> stmt;
symbol seq : stmt × stmt -> stmt;
symbol if : expr × stmt × stmt -> stmt;
symbol while : expr × stmt -> stmt;

-- Expressions
symbol intconst : integer -> expr ;
symbol boolconst : bool -> expr ;
symbol var : string -> expr ;
symbol unary: uop × expr -> expr ;
symbol binary : bop × expr × expr !->expr ;

-- Unary operators
symbol negate : -> uop;
symbol not : -> uop;

-- Binary operators
symbol add: -> bop;
symbol sub: -> bop;
symbol mul: -> bop;
symbol lt : -> bop;
symbol leq : -> bop;
symbol eq: -> bop;
symbol geq: -> bop;
symbol gt : -> bop;
symbol and: -> bop;
symbol or : -> bop;
```

### Standard presentation of BIPL in Haskell

The BIPL object language’s AST representation in Haskell uses a distinct data type for expr and for stmt from the previous algebraic signature.
```haskell
-- Representing object language BIPL in (the metalanguage) Haskell:
-- Statements and Expressions are distinct types
-- Statements
data Stmt
  = Skip
  | Assign String Expr
  | Seq Stmt Stmt
  | If Expr Stmt Stmt
  | While Expr Stmt
  
-- Expressions
data Expr
  = IntConst Int
  | BoolConst Bool
  | Var String
  | Unary UOp Expr
  | Binary BOp Expr Expr
  
-- Unary and binary operators
  data UOp = Negate | Not
  data BOp = Add | Sub | Mul | Lt | Leq | Eq | Geq | Gt | And | Or
```

This too is a trade-off; it on the one hand makes it impossible to represent ASTs that could not be generated by the signature, but on the other hand may complicate further processing of the AST: code that traverses ASTs must “hop” between different data types.

### Merged presentation of BIPL in the Haskell

One might thus choose to combine the sorts of expressions and statements. This alternative, shown below represents expressions and statements with the same data type. We retain the distinct type names `Expr` and `Stmt` by adding Stmt as an alias for `Expr`.
```haskell
-- Representing object language BIPL in (the metalanguage) Haskell:
-- Statements and Expressions are the same type (with different names)
type MExpr = MStmt

data MStmt
  = MSkip
  | MAssign String MExpr
  | MSeq MStmt MStmt
  | MIf Expr MStmt MStmt
  | MWhile MExpr MStmt
  | MIntConst Int
  | MBoolConst Bool
  | MVar String
  | MUnary UOp MExpr
  | MBinary BOp MExpr MExpr
  deriving ( Show, Eq, Read )
  
-- Unary and binary operators
data UOp = Negate | Not
data BOp = Add | Sub | Mul | Lt | Leq | Eq | Geq | Gt | And | Or

-- Checking if a merged BIPL AST represents a statement
isStmt :: MStmt −> Bool
isStmt (MSkip) = True
isStmt (MAssign _ _) = True
isStmt (MSeq _ _) = True
isStmt (MIf _ _ _) = True
isStmt (MWhile _ _) = True
isStmt _ = False

-- Checking if a merged BIPL AST represents an expression
isExpr :: MStmt −> Bool
isExpr = not . isStmt
```

### Interchange formats

We mention that sometimes ASTs end up being represented using interchange formats, such as JSON or various XML languages. The running example of ours is below encoded as JSON. Interface formats have their own requirements that ASTs must conform to. For example, JSON does not have tuple types, only lists and records. Representing the three branches of `if` as fields of a record thus necessitates generating (artifical) names for those branches, to be used as field labels.
```c++
{
  "pred": {
    " if ": {
      "x": { " iszero ": { "zero": { } } },
      "y": { "succ": { "succ": { "zero": { } } } },
      "z": { "zero": { } }
  } }
}
```


## Lambda Calculus
A mathematical discipline consisting purely of lambda functions.
Values like ints and booleans are included only if represented as lambdas.

### Lambda booleans
To represent a boolean as a function, try to think of the functionality you need whenever you need boolean and represent that instead.
A boolean can be function that takes two arguments, and returns either the first or the second.

type Bool = a -> a -> a

true :: Bool \
true = \a -> \b -> a

false :: Bool \
false = \a -> \b -> b

An if-expression can then be represented as \
if :: Bool -> a -> a \
if = \b -> \e1 -> \e2 -> b e1 e2 \
where b is a boolean like described above. If b is true, e1 will be returned, otherwise e2 is returned.

Note that there is a difference between if-expression and an if-statement:
- if-expression: 'the value of this expression is either this or that', like if then else i Haskell
- if-statement: 'either do this or do that', like if-else in every other language

#### Boolean operators

Operators like and, or, not are relatively simple to represent:

and :: Bool -> Bool -> Bool \
and = \b1 -> \b2 -> b1 b2 false

or :: Bool -> Bool -> Bool \
or = \b1 -> \b2 -> b1 true b2

not :: Bool -> Bool \
not \b -> b false true

### Lambda numbers
To represent numbers as functions, try first to answer the question: when programming, when do you ever need a natural number n if not to do something n times? 
The answer is 'quite often', but let's pretend the answer is 'never' and move on. Let us try to represent the functionality of doing something n times as a function:

type Number = (a -> a) -> a -> a 

A natural number n is therefore a function that takes another function and an argument, and applies the function to the argument n times. The first numbers then look like:

zero :: Number \
zero = \f -> \x -> x

one :: Number \
one = \f -> \x -> f x

two :: Number \
two =  \f -> \x -> f (f x)

We only need to hardcode zero, as the rest can be represented as applying the succ function to zero n times:

succ :: Number -> Number \
succ = \n -> (\f -> \x -> f (n f x))

You may read that as the succ function taking a number n, a function f, an argument x, uses n to apply f to x n times, and applies f one more time afterwards. The naturaly numbers are therefore just zero, succ zero, succ (succ zero), etc.

### Number operators
The operators add and multiply are also relatively simple:

add :: Number -> Number -> Number \
add = \n -> \m -> \f -> \x -> n f (m f x)

multiply :: Number -> Number -> Number \
multiply = \n -> \m -> \f -> \x -> n (m f) x

However, the operators to subtract and divide are far more complicated, and has been left as an excercise to the reader :P




