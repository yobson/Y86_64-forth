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

## Using
### Building
You will need a modern version of GHC and cabal. Simply run `make` in the code directory.

### The Interpreter
To use the interpreter (I assume a unix system), simply run `rlwrap ./FORTH`. Do not be thrown by the lack of prompt!

### The Compiler
I am going to completely change how the compiler is used (because it is a little inconvenient at time of writing) so watch this space.

Assuming your code is in the file `code.f`, run `cat code.f | ./FORTH c`. The output will be printed on the screen. Copy this into
[this simulator](http://www.cs.ox.ac.uk/people/alex.rogers/Y86-64/) and hit run. The stop of the stack at the end of the
simulation is denoted by `< RSP` and the bottom is at `0x01f8`
