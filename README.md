# Y86_64 FORTH

This is a simple compiler and interpreter for a simple dialect of FORTH. It compiles to Y86\_64,
specifically for [this](http://www.cs.ox.ac.uk/people/alex.rogers/Y86-64/) simulator used in
the architecture course at Oxford University. I made this primarily to revise both architecture and
compilers simultaneously.

## The Language
The language is very simple. You in put a list of symbols (white space separated) they are pushed to the stack in order.
You can also push a function (user defined or primitive) which will pop its input of the stack and push the result onto
the stack. Functions are defined by starting with a colon, then the function name, the optionally in curly brackets
you can pattern match the top of the stack (left is the top). If statements use curly brackets to denote the if and else part.

The grammar is as follows:
```
Exp  : SExp Exp                    { $1 :> $2 }
     | SExp                        { $1 }

SExp : num                          { Number $1 }
     | id                           { Variable $1 }
     | prim                         { Prim $1 }
     | nop                          { Nop }
     | if '{' Exp '}' '{' Exp '}'   { If $3 $6 }
     | ':' id Exp ';'               { Closure $2 [] $3 }
     | ':'  id '{' List '}' Exp ';' { Closure $2 $4 $6 }

List : id                           { [$1] }
     | id List                      { $1:$2 }
```
Because learning a language from grammars is not always ideal, here is an example program that finds the
9th Fibonacci number
```forth
:dup {a} a a;
:del {a} _;
:swap {a b} a b;

:fib dup 0 == IF {_} {dup 1 == IF {_} {dup 1 - fib swap 2 - fib +}};

9 fib
```
For a more comprehensive show of the language, see `test.f`

## Using
### Building
You will need a modern version of GHC and cabal. Simply run `make` in the code directory.

### The Interpreter
- To use the interpreter (I assume a unix system), simply run `./FORTH -i` or `./FORTH --interactive`.
- You can give a files as input and it will load in the functions defined in them and prepare the stack.

When the stack grows, the `^` symbol will point to the stop of the stack.

### The Compiler
Assuming your code is in the file `code.f`, run `./FORTH -o OutFile code.f`. If you do not specify an output, the code
will be printed to stdout. You can turn off optimisation with `-n` or `--no-opt` although it will still do some inlining.

To run the code, copy it into
[this simulator](http://www.cs.ox.ac.uk/people/alex.rogers/Y86-64/) and hit run. The stop of the stack at the end of the
simulation is denoted by `< RSP` and the bottom is at `0x01f8`

To test the compiler, run `./FORTH test.f`

## Quirks
The order you give the files to the compiler (for now) matters if you don't use `#include file` directives.
If file `A` relies on functions in file `B`, write `./FORTH B A`
