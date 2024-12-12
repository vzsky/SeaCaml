# notes: 

## Issues:
- C does provide implicit casting (which is needed in the example)
    - There are so many cases to consider. So we will ONLY do implicit casting for assignment statement.
    - Will consider only `int <-> float`
    - Implicit Casting is painful

## To run 
- main ocaml: `dune exec ./src/main.exe ./example/assume.c`
- for an assembly: `gcc print.asm -o print`

## potentially to do: 
- Expand the language AST and statement
- statements and expression can be merged!? no need to separate
