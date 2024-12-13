# SeaCaml

A minimal C interpretor. 

## Statement Recognition: 
currently limit to a small subset of C. To be specific, we only have a few statements revolving `int, float, bool` and string literal (not even `char`)
- declaration `int x, y, z = 2, w[100];`
- assigment `x = 1;`
- binary operation including `+, -, *, /` and comparisons
- unary operation including `-, !`
- functions definition `int func (int x) { return 0; }` and function call `func(5);` with some builtin
- conditioning `if (x) y else z`
- looping with `for` and `while` without `continue` or `break`
- incremental `x++`
- compound `x += 1`, `x -= 1`

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

Or build using 
```
dune build
mv _build/default/src/main.exe ./SeaCaml
```

and finally run with 
```
./SeaCaml ./example/assume.c
```

A few examples are given in the repository.
