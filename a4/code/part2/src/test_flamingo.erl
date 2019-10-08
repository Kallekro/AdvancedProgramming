-module(test_flamingo).

-include_lib("eunit/include/eunit.hrl").
-export([mytests/0]).

mytests() ->
    eunit:test({module, ?MODULE}, [verbose]).

start_a_flamingo_server_test() ->
    ?assertMatch({ok, _}, flamingo:start("flamingo")).

generator_start_a_flamingo_server_test_() ->
    ?_assertMatch({ok, _}, flamingo:start("flamingo")).

fancy_generator_start_a_flamingo_server_test_() ->
    {"Start a flamingo server an check that it returns a pair with ok as the first component",
     ?_assertMatch({ok, _}, flamingo:start("flamingo"))}.

%% failing_test_() ->
%%     {"We messed up the test",
%%      ?_assertMatch({{ok}, _}, flamingo:start("flamingo"))}.

greetings_test_() ->
    {"Start a greeting server, and send a request",
     fun () ->
             F = greetings:server(),
             Ref = make_ref(),
             flamingo:request(F, {"/hello", [{"name", "Ken"}]},
                              self(), Ref),
             Expected = "Greetings Ken\nYou have reached The Flamingo Server",
             receive
                 X ->
                     ?assertMatch({Ref, {200, _, Expected}}, X)
             end
     end}.
