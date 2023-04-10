-module(numbers).
-export([fib/1]).
-export([fact/1]).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

fact(0) -> 1;
fact(1) -> 1;
fact(N) -> N * fact(N - 1).