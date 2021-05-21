# Calculator with variables

## Interpreter for calculator with variables

This interpreter is very similar to our previous solutions:
- It uses state instead of store.
- It has replaced the calls to store with relevant calls to state.

Otherwise the pattern for the interpreter functions `evaluate` and `execute` are the same as
before.

## Unit Test

The unit test `unittestCalculatorVariableInterpreter` and the interactive calculator `main`
have seen larger changes. The problem is that we now need to be careful when building the
example ASTs:
- Variables need to be declared using `SetVar` before being used. Previously we could
access any of the registers any time since we would retrieve a 0 if no value had been set.
- Changing a defined variable needs to use `AssVar` .

And of course the names of the functions for modifying the state are different from those of
manipulating the store.

In spite of these differences, the similarity between the previous and the current set of
codes should be fairly obvious.
