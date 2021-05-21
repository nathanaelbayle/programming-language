# Calculator for more domains

### CalculatorValueDomainAST.hs
Contains the AST data type for a value domain independent calculator. 
This also includes the infrastructure for lists of primitive operations
for the calculator with support functions and unit test `unittestprimitives` . There is
also an example declaration of primitive functions for a boolean calculator with unit
test `unittestboolprimitives`. The latter can be used as a pattern for checking the
completeness of other value domain calculators.

### CalculatorValueDomainState.hs
Contains types `State`, `Environment`, `Store` with support functions for a 
value domain independent calculator. This is an upgrade exactly as described in the tasks.
It includes upgraded unit test `unittestCalculatorState`.

### CalculatorValueDomainInterpreter.hs
Contains an interpreter for expressions and
statements for a value domain independent calculator. This is a straight forward upgrade
according to the task description. It includes `unittestCalculatorValueDomainInterpreter` ,
an upgraded unit test. The interactive calculator support has been moved to its own
file `CalculatorValueDomainMain.hs`.

### CalculatorValueDomainMain.hs
Contains support functions for an interactive value domain independent
calculator. This collates `mainCalculatorPrint` from the old integer calculator and `mainCalculatorVariable` 
from the old interpreter, and their support functions, upgraded to the new value domain independent infrastructure. 
It uses the Haskell `read` function to parse the input data.

### CalculatorValueDomainInteger.hs 
Contains an integer calculator using the value domain independent calculator infrastructure. Specifically `main` calls the new interactive
calculator infrastructure which is collated in the file `CalculatorValueDomainMain.hs`.

The file also includes an extended unit test `unittestCalculatorIntegerPrimitives` . The
first part calls `testapplication` which runs the semantic function on the list of primitve
functions to check that they all have been given a semantics, and the result of these
applications are checked against a list of expected results as in traditional unit testing.
The second part runs a selection of `integerproperties` on larger data sets. Here integer
properties states that the Abs function should always give a positive result, and that
`n1 = (n1 idiv n2) ∗ n2 + n1 mod n2 `for all integers `n1, n2` where `n != 0`. The integer
properties are parameterised unit tests, and can therefore be tested on large amounts of
(possibly randomised) data.

### CalculatorValueDomainReal.hs 
Contains a real calculator using the value domain independent calculator infrastructure.

The unit test `unittestCalculatorRealPrimitives` first checks that all primitive functions
have been given a semantics with expected results. It then runs a larger set of parameterised unit tests checking
`Abs` , square/square root, logartihm/exponentiation, and
integer division/remainder. Since these use floating point computations the tests are
sensitive to the input data which therefore have been semi-carfully selected.
