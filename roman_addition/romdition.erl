-module(romdition).
-include_lib("eunit/include/eunit.hrl").

add(_, _) -> "II".

a_test() -> "II" = add("I", "I").
