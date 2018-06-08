-module(romdition).
-include_lib("eunit/include/eunit.hrl").

add(A, B) -> collapse(string:join([A, B], "")).

collapse([$I, $I, $I, $I | Tail]) -> "IV";
collapse([Head | Tail]) -> [Head | Tail].

a_test() -> "II" = add("I", "I").
b_test() -> "III" = add("I", "II").
c_test() -> "III" = add("II", "I").
d_test() -> "IV" = add("II", "II").
e_test() -> "IV" = add("I", "III").
f_test() -> "IV" = add("III", "I").
