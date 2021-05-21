# Calculator with register variables

## Interpreter for calculator with register variables
This interpreter is identical to the one in pamphlet two, except that:
- The data type `Register` has been removed.
- All register names from the enumeration have been changed to corresponding strings.

The latter also holds for the function `getregisterindex`. Now the inactive last case can also
be activated. It will now match any inapproriate string used in place of a register name.
````haskell
getregisterindex reg = error $ "Unknown␣register␣" ++ (show reg)
````

## Unit test and interactive calculator with register variables

The only difference from Pamphlet 2 for the unit tests and interactive calculator is
that register names `Reg0`, etc now are strings `"Reg0"`, etc.
