-module(lists).
-export([sequence/1]).
-export([rev_sequence/1]).
-export([print_list/1]).

sequence(1) -> 1;
sequence(N) -> [sequence(N - 1) | N].

rev_sequence(1) -> 1;
rev_sequence(N) -> [N | rev_sequence(N - 1)].

print_list([X]) ->
    io:format("~p", [X]);
print_list([X | Tail]) ->
    io:format("~p", [X]),
    print_list(Tail).