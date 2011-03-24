-module(peter_eriksson_1).
-author("that.peter@gmail.com").
-export([convert/1, examples/0]).

-define(TABLE, [{1000, "M"}, {900, "CM"}, {500, "D"}, {400, "CD"},
                               {100, "C"}, {90, "XC"}, {50, "L"}, {40, "XL"},
                               {10, "X"}, {9, "IX"}, {5, "V"}, {4, "IV"},
                               {1, "I"}]).

convert(Number) when Number > 0, Number =< 3000 ->
   convert(Number, ?TABLE).

convert(_, []) -> [];
convert(Number, [{Val, _}|T]) when Number < Val -> convert(Number, T);
convert(Number, [{Val, Roman}|T]) ->
    NewNumber = Number - Val,
    [Roman|convert(NewNumber, [{Val, Roman}|T])].

% Demo
examples() -> examples([1, 10, 100, 1000, 12, 123, 1234, 1990, 2008, 3000]).

examples([Number|Samples]) ->
    io:format("~w \t\t= ~s~n", [Number, convert(Number)]),
    examples(Samples);
examples([]) -> ok.