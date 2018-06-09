-module(romdition).
-include_lib("eunit/include/eunit.hrl").

add(A, B) -> sumIs(lists:flatten([toIs(A, []), toIs(B, [])]), []).

toIs([$I, $V | Tail], Expanded) -> toIs(Tail, ["IIII" | Expanded]);
toIs([$V | Tail], Is) -> toIs(Tail, ["IIIII" | Is]);
toIs([C | Tail], Expanded) -> toIs(Tail, [C | Expanded]);
toIs([], Expanded) -> lists:flatten(Expanded).

sumIs([$I, $I, $I, $I, $I, $I, $I, $I, $I | Tail], Sum) -> sumIs(Tail, ["IX" | Sum]);
sumIs([$I, $I, $I, $I, $I | Tail], Sum) -> sumIs(Tail, ["V" | Sum]);
sumIs([$I, $I, $I, $I | Tail], Sum) -> sumIs(Tail, ["IV" | Sum]);
sumIs([Head | Tail], Sum) -> sumIs(Tail, lists:flatten(lists:append(Sum, [Head])));
sumIs([], Sum) -> lists:flatten(Sum).

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
k_test() -> "VI" = add("IV", "II").
l_test() -> "VII" = add("I", "VI").
m_test() -> "IX" = add("IV", "V").
% -- This is as far as we got during the mob programming workshop
