-module(test_async).
-export([test_wait_pos/0, test_wait_exception/0, test_poll_pos/0, test_poll_exception/0, test_wait_catch_pos/0, test_wait_catch_exception/0, test_wait_any_pos/0]).

sqr(X) -> X * X.
sleepThenSqr(X) -> timer:sleep(1000), sqr(X).
sleepForX(X) -> timer:sleep(X), true.
except(X) -> throw(X).

test_wait_pos() ->
    Aid = async:new(fun sqr/1, 2),
    async:wait(Aid) =:= {ok, 4}.

test_wait_exception() ->
    Aid = async:new(fun except/1, test_exception),
    try async:wait(Aid), false
    catch test_exception -> true;
          _ -> false
    end.

test_poll_pos() ->
    Aid = async:new(fun sleepThenSqr/1, 2),
    A = async:poll(Aid),
    timer:sleep(500),
    B = async:poll(Aid),
    timer:sleep(700),
    C = async:poll(Aid),
    {A, B, C} =:= {nothing, nothing, {ok, 4}}.

test_poll_exception() ->
    Aid = async:new(fun except/1, test_exception),
    timer:sleep(500),
    async:poll(Aid) =:= {exception, test_exception}.

test_wait_catch_pos() ->
    Aid = async:new(fun sqr/1, 2),
    async:wait_catch(Aid) =:= {ok, 4}.

test_wait_catch_exception() ->
    Aid = async:new(fun except/1, test_exception),
    async:wait_catch(Aid) =:= {exception, test_exception}.

test_wait_any_pos() ->
    Aid1 = async:new(fun sleepForX/1, 1000),
    Aid2 = async:new(fun sleepForX/1, 500),
    Aid3 = async:new(fun sleepForX/1, 50),
    Aid4 = async:new(fun sleepForX/1, 500),
    async:wait_any([Aid1, Aid2, Aid3, Aid4]) =:= {Aid3, true}.