# Calculator with registers

## Interpreter for a register based calculator
The interpreter has three functions to deal with the semantics of the register based calculator AST.
- `evaluate :: CalcExprAST −> Store −> Integer`

  to evaluate a calculator expression given a store.
- `execute :: CalcStmtAST −> Store −> Store`

  to set the value of a calculator expression to a register in the store.
- `getregisterindex :: Register −> Integer`

  to map a register to an index in the store.
  
In the code listing an extra case is commented away for each of the semantic functions. These cases 
captures any missing alternatives in the case distinctions, but only gives 
a “scolding” by the Haskell compiler: “warning: [-Woverlapping-patterns] Pattern match is redundant”.

In addition to the interpreter, there is a unit test and an interactive register based calculator.

## Unit test
The unit test `unittestCalculatorRegisterInterpreter` checks:
- that the named expressions `calculatorRegisterAST1 `, . . . , evaluate as expected,
- that a sequence of register updates gives an expected store `( store == store3)` , and
- that the named statements `calculatorSetRegisterAST1` , . . . , execute with the expected change to the start register.

## Interactive register calculator

The interactive register based calculator is started by calling `main`. It understands three commands:
- The empty string: stops the interactive calculator.
- The command “show”: prints the current value of the store.
- A `CalcStmtAST` statement which is parsed and executed (and updates the store).

The interactive calculator repeats the prompt “c” until it crashes (any mispelling of the statement AST) or stops. Try the commands
````haskell
SetReg Reg4 ( Lit 4)
SetReg Reg1 (Neg (Mult (Add (Lit 3) (Sub ( Lit 7) ( Lit 13))) ( Lit 19))) SetReg Reg2 (Add (Reg Reg1) (Reg Reg4))
SetReg Reg1 (Reg Reg2)
SetReg Reg1 (Add (Reg Reg1) (Reg Reg4))
show
SetReg Reg1 (Add (Reg Reg1) (Reg Reg4))
show
````

A final blank line will stop the interactive calculator with the message “Finished”.

The interactive calculator is split into several functions.
- `main` which starts by calling `mainCalculatorRegisterAsk` with a freshly initialised store.
- `mainCalculatorRegisterAsk` which outputs a prompt, gets a line of input using `getLine`,
and decides whether to show the state of the registers or to execute a calculator instruction updating the registers or to stop.
- `mainCalculatorRegisterShowstate` which prints the state of the registers (the `Store`),
before recursively calling `mainCalculatorRegisterAsk`.
- `mainCalculatorRegisterExc` which parses the input string as a register calculator statement `CalcStmtAST`
using `read` (as before). It then prints and evaluates the statement’s
expression, before calling `mainCalculatorRegisterAsk` recursively with the updated store.

As previously noted, the `getLine` input routine does not allow string editing on the
terminal, and the read function will fail disgracefully if the input string does not match the
`CalcStmtAST` syntax.

