:dup {a} a a;
:del {a} _;
:swap {a b} a b;
:rot2 swap;
:rot3 {a b c} a c b;

:fib dup 0 == IF {_} {dup 1 == IF {_} {dup 1 - fib swap 2 - fib +}};

2 fib 1 fib 0 fib
