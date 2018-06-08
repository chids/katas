-module(romdition).
-include_lib("eunit/include/eunit.hrl").

add(A, B) -> string:join([A, B], "").

a_test() -> "II" = add("I", "I").
b_test() -> "III" = add("I", "II").
c_test() -> "III" = add("II", "I").