-module(romdition).
-include_lib("eunit/include/eunit.hrl").

add(A, B) -> sumIs(lists:flatten([toIs(A, []), toIs(B, [])])).

toIs([$I, $V | Tail], Expanded) -> toIs(Tail, ["IIII" | Expanded]);
toIs([C | Tail], Expanded) -> toIs(Tail, [C | Expanded]);
toIs([], Expanded) -> lists:flatten(Expanded).

sumIs([$I, $I, $I, $I, $I | _]) -> "V";
sumIs([$I, $I, $I, $I]) ->  "IV";
sumIs([Head | Tail]) -> [Head | Tail].

a_test() -> "II" = add("I", "I").
b_test() -> "III" = add("I", "II").
c_test() -> "III" = add("II", "I").
d_test() -> "IV" = add("II", "II").
e_test() -> "IV" = add("I", "III").
f_test() -> "IV" = add("III", "I").
g_test() -> "V" = add("II", "III").
h_test() -> "V" = add("III", "II").
i_test() -> "V" = add("I", "IV").
j_test() -> "V" = add("IV", "I").
