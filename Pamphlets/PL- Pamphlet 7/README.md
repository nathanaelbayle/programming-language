# Documentation strings

## Modified AST for external primitives with documentation strings
The recommended changes in pamphlet 7 are easy to integrate in the AST module. Here we
have also cleaned up the module by removing the AST examples.

## Interpreter for external primitives with documentation strings
There is nothing to update in the interpreter file except possibly upgrading module names. It
is therefore not listed here.

## Modified module for primitives with documentation strings
The changes to ``Calculator8IntegerPrimitives`` are sketched in pamphlet 7.

This listing also incorporates the modification of ``main`` to print the documentation strings.
The modifications are detailed in the next subsection.

## Modified ``main`` for printing documentation strings
Changing the printing of the primitive functions to also include the documentation string
requires some thinking. Above we first added a function call ( `printdoc doc`) and concatenated
that to the printing of the function declaration. Then the approach (foldl on the map of the
`integeroperations` ) to ensure we print all primitive function declarations will include the
documentation.

Since each line of the documentation needs to be printed on a separate line with the prefix
`−−| `, we need to be able to split the multilines in `doc` into a list of lines. The `spliOn`
function does this for any list. Recall that a string, like `doc` , is a list of characters, so we can
split the string on character `’\n’` to split `doc` into a list of lines.

The three lines in the definition of `printdoc` doc do the following:
- Split `doc` into a list on single lines.
- Prepend `−−|` to each line of `doc` , and place a new line symbol as suffix (ensures Haskell
will print this as one line).
- Concatenate the list of lines into one long string. The Haskell print system ensures that
the new line symbol starts a new print line, even if the newline symbol is in the middle
of a string.
