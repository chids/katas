% "Write a program that prints the numbers from 1 to 100.
% But for multiples of three print “Fizz” instead of the number
% and for the multiples of five print “Buzz”.
% For numbers which are multiples of both three and five print “FizzBuzz”."
% 
% - http://c2.com/cgi/wiki?FizzBuzzTest
%
% Run with:
% erlc fizzbuzz1.erl && erl -run fizzbuzz1 run -run init stop -noshell

-module(fizzbuzz1).
-export([run/0]).
-define(START, 1).
-define(END, 100).

run() ->
    print_range(?START).

print_range(Number) when Number =< ?END ->
    print_number(Number, [{3, "Fizz"}, {5, "Buzz"}], true),
    print_range(Number + 1);
print_range(_) ->
    false.

print_number(Number, [Pair|NextPair], PrintNumber) ->
    {Multiple, Word} = Pair,
    case (Number rem Multiple) of
        0   -> io:fwrite(Word), print_number(Number, NextPair, false);
        _   -> print_number(Number, NextPair, (PrintNumber and true))
    end;
print_number(_Number, [], false) ->
    io:format("~n", []);
print_number(Number, [], true) ->
    io:format("~w~n", [Number]).
