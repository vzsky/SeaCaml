# SeaCaml

A minimalistic C interpretor. 

## Statement Recognition: 
I currently limit the feature to a small subset of C. To be specific, we only have a few statements revolving `int, float, bool` and `string`
- declaration `int x, y, z;`
- assigment `x = 1;`
- binary operation including `+, -, *, /` and comparisons
- unary operation including `-, !`
- functions definition `int func (int x) { return 0; }` and function call `func(5);` with some builtin
- conditioning `if (x) { y }` without `else`
- looping `for (x;y;z) { w }` without `while`, `break`, and `continue`
- incremental `x++`

### Builtin Functions
- `printf(string, any list)`
- `println(any)`
#### For devs
- `debug_println(any)` - a fail safe version of println
- `debug_print_context()` - show the current context of interpretation

## Notes
 - I implemented a limited version of `printf` which works minimally for `%d, %f` and probably nothing else. Due to type system of Ocaml, the usage of `printf` is unnatural and opting for `println` might be better.
 - `string` is not a native element in this language (at least not yet). One could only declare string literal `"something"` but could not alter it as we don't currently have a `char` supported.

# Run
dune should do its job.
```
dune exec ./src/main.exe ./example/assume.c
```
