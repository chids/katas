% "Write a program that prints the numbers from 1 to 100.
% But for multiples of three print “Fizz” instead of the number
% and for the multiples of five print “Buzz”.
% For numbers which are multiples of both three and five print “FizzBuzz”."
% 
% - http://c2.com/cgi/wiki?FizzBuzzTest
%
% Run with:
% erlc fizzbuzz2.erl && erl -run fizzbuzz2 run -run init stop -noshell

-module(fizzbuzz2).
-export([run/0]).
-define(START, 1).
-define(END, 100).

run() ->
    print_range(lists:seq(?START, ?END)).

print_range([Number|Remaining]) ->
    case { (Number rem 3) =:= 0, (Number rem 5) =:= 0 } of
        {true, true}    -> io:fwrite("FizzBuzz\n");
        {true, false}   -> io:fwrite("Fizz\n");
        {false, true}   -> io:fwrite("Buzz\n");
        _               -> io:format("~w~n", [Number])
    end,
    print_range(Remaining);
print_range([]) ->
    ok.