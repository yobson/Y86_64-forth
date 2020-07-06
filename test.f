#include "lib.f"

-- There isn't enough space for both of these in the emulator
:fib dup 0 == IF {_} {dup 1 == IF {_} {dup 1 - fib swap 2 - fib +}};

-- :fib dup 0 == IF {_} {dup 1 == IF {0 TMP !} {1 - fib dup TMP @ + swap TMP !}};

9 fib
