-module(p99).
-export([last/1]).
-export([penultimate/1]).
-export([kth/2]).
-export([len/1]).
-export([reverse/1]).
% -export([reverse_deep/1]).
-export([is_palindrome/1]).
-export([flatten/1]).
-export([compress/1]).
-export([pack/1]).
-export([encode/1]).
-export([encodeModified/1]).
-export([decode/1]).
-export([encodeDirect/1]).
-export([duplicate/1]).
-export([duplicateN/2]).
-export([drop/2]).
-export([split/2]).
-export([slice/3]).
-export([rotate/2]).
-export([removeAt/2]).
-export([insertAt/3]).
-export([range/2]).
-export([randomSelect/2]).
-export([lotto/2]).
-export([randomPermute/1]).

last([X]) -> X;
last([_ | T]) -> last(T).

penultimate([X, _]) -> X;
penultimate([_ | T]) -> penultimate(T).

kth([H | _], 0) -> H;
kth([_ | T], K) -> kth(T, K - 1).

len([]) -> 0;
len([_ | T]) -> 1 + len(T).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

% reverse_deep([]) -> [];
% reverse_deep([[H | T] | Xs]) -> reverse_deep(Xs) ++ [reverse_deep([H | T])];
% reverse_deep([H | T]) -> reverse_deep(T) ++ [H].

is_palindrome([]) -> true;
is_palindrome(List) -> List == reverse(List).

% Not sure what the intended behavior for this one is:
% 24> problems_99:flatten([1, [1, [2, 1]], []]). 
% [1,1,2,1,[]]
flatten([]) -> [];
flatten([[X | Xs] | T]) -> flatten([X | Xs]) ++ flatten(T);
flatten([X | Xs]) -> [X] ++ flatten(Xs).

% Really terrible solution
compress_inner([], _) -> [];
compress_inner([H | T], H) -> compress_inner(T, H);
compress_inner([H | T], _) -> [H] ++ compress_inner(T, H).
compress([]) -> [];
compress([X]) -> [X];
compress([H | T]) -> [H] ++ compress_inner(T, H).

% [1, 1, 2, 3, 3] -> [[1,1], [2], [3, 3]]
% [5] -> [[5]]
% [1, 2, 1] -> [[1], [2], [1]]

% pack_consume will consume the input list until all A values have been consumed,
% then return the remainder of the list
pack_consume(A, [A | T]) -> pack_consume(A, T);
pack_consume(_, []) -> [];
pack_consume(_, Remainder) -> Remainder.
% pack_repeating will return a sublist containing contiguous values of A
pack_repeating(A, [A | T]) -> [A] ++ pack_repeating(A, T);
pack_repeating(_, _) -> [].

pack([]) -> [];
pack([A, A | T]) -> 
    Repeated = [A] ++ pack_repeating(A, [A | T]),
    Remainder = pack_consume(A, [A, A | T]),
    [Repeated | pack(Remainder)];
pack([H | T]) -> [[H]] ++ pack(T).

% Run length encoding
% [1, 1, 5] -> [{2, 1}, [1, 5]]
encode_element([]) -> [];
encode_element([[X | Xs] | T]) -> [{1 + len(Xs), X}] ++ encode_element(T);
encode_element([H | T]) -> {1 + len(T), H}.
encode([]) -> [];
encode(List) -> encode_element(pack(List)).

encodeModified_element([]) -> []; 
encodeModified_element([[X] | T]) -> [X] ++ encodeModified_element(T);
encodeModified_element([[X | Xs] | T]) -> [{1 + len(Xs), X}] ++ encodeModified_element(T).
encodeModified([]) -> [];
encodeModified(List) -> encodeModified_element(pack(List)).

decode_repeat(0, _) -> [];
decode_repeat(Length, Value) -> [Value] ++ decode_repeat(Length - 1, Value).

% [{2, 1}, [1, 5]] -> [1, 1, 5]
decode([]) -> [];
decode([{Length, Value} | T]) -> decode_repeat(Length, Value) ++ decode(T).

encodeDirect_inner([], _, Accumulator) -> Accumulator;
encodeDirect_inner([H | T], H, [{Length, H}]) -> encodeDirect_inner(T, H, [{Length + 1, H}]);
encodeDirect_inner([H | T], _, Accumulator) -> Accumulator ++ encodeDirect_inner([H | T], H, [{0, H}]).
encodeDirect([]) -> [];
encodeDirect([H | T]) -> encodeDirect_inner([H | T], H, [{0, H}]).

duplicate([]) -> [];
duplicate([H | T]) -> [H, H] ++ duplicate(T).

duplicateN_fill(_, 0) -> [];
duplicateN_fill(Value, N) -> [Value] ++ duplicateN_fill(Value, N - 1). 
duplicateN([], _) -> [];
duplicateN([H | T], N) -> duplicateN_fill(H, N) ++ duplicateN(T, N).

% Drop every Nth element of list
drop_inner([], _, _) -> [];
drop_inner([_ | T], 1, N) -> drop_inner(T, N, N);
drop_inner([H | T], Count, N) -> [H] ++ drop_inner(T, Count - 1, N).
drop([], _) -> [];
drop(List, N) -> drop_inner(List, N, N).

split_inner(0, List, Accumulator) -> [Accumulator, List];
split_inner(N, [H | T], Accumulator) -> split_inner(N - 1, T, Accumulator ++ [H]).
split(0, []) -> [];
split(N, List) -> split_inner(N, List, []).

slice(0, 0, _) -> [];
slice(0, End, [H | T]) -> [H] ++ slice(0, End - 1, T);
slice(Start, End, [_ | T]) -> slice(Start - 1, End - 1, T).

% Rotate a list N places to the left.
% This is a fragile solution that cannot handle rotations greater than length of the list
rotate_inner(0, List, Accumulator) -> List ++ Accumulator;
rotate_inner(N, [H | T], Accumulator) -> rotate_inner(N - 1, T, Accumulator ++ [H]).
% rotate(0, List) -> List;
rotate(N, List) -> rotate_inner(N, List, []).
% rotate(N, List) -> rotate_inner(mod(N, len(List)), List, []).

removeAt_inner(0, [H | T], Accumulator) -> {Accumulator ++ T, H};
removeAt_inner(N, [H | T], Accumulator) -> removeAt_inner(N - 1, T, Accumulator ++ [H]).
removeAt(K, List) -> removeAt_inner(K, List, []).

insertAt_inner(Value, 0, List, Accumulator) -> Accumulator ++ [Value] ++ List;
insertAt_inner(Value, N, [H | T], Accumulator) -> insertAt_inner(Value, N - 1, T, Accumulator ++ [H]).
insertAt(Value, N, List) -> insertAt_inner(Value, N, List, []).

% Breaks when first index is greater than second
range(X, X) -> [X];
range(First, Last) -> range(First, Last - 1) ++ [Last].

% Extract a given number of randomly selected elements from a list.
randomSelect(0, _) -> [];
randomSelect(N, List) -> 
    {Remaining_list, Element} = removeAt(rand:uniform(len(List)) - 1, List),
    [Element] ++ randomSelect(N - 1, Remaining_list).
% Do you think I could have done this in fewer lines

% Lotto: Draw N different random numbers from the set 1..M.
lotto(N, M) -> randomSelect(N, range(1, M)).

randomPermute(List) -> randomSelect(len(List), List).