-module(list_stuff).
-export([sequence/1]). % Creates a sequence from 1...N
-export([rev_sequence/1]). % Creates a sequence from N...1
-export([print_list/1]). % Prints list with extra spacing
-export([reverse/1]). % Reverses list
-export([reverse_deep/1]). % Reverses list and all sublists
-export([reverse_deep/2]). % Reverses until a certain recursive depth


sequence(1) -> [1];
sequence(N) -> sequence(N - 1) ++ [N].

rev_sequence(1) -> 1;
rev_sequence(N) -> [N] ++ rev_sequence(N - 1).

print_list_rec([X]) ->
    io:format("~p ]\n", [X]);
print_list_rec([H | T]) ->
    io:format("~p, ", [H]),
    print_list_rec(T).

print_list([]) ->
    io:format("[]\n");
print_list([X]) ->
    io:format("[ ~p ]\n", [X]);
print_list([H | T]) ->
    io:format("[ ~p, ", [H]),
    print_list_rec(T).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

reverse_deep(X, 0) -> reverse(X);
reverse_deep([], _) -> [];
reverse_deep([[X | Xs] | T], N) ->
    reverse_deep(T, N) ++ [reverse_deep([X | Xs] , N - 1)];
reverse_deep([H | T], N) -> 
    reverse_deep(T, N) ++ [H].

reverse_deep([]) -> [];
reverse_deep([[X | Xs] | T]) ->
    reverse_deep(T) ++ [reverse_deep([X | Xs])];
reverse_deep([H | T]) -> 
    reverse_deep(T) ++ [H].