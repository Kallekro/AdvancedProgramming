-module(test_rps).
-export([test_all/0, match_2_players/0]).

%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").


test_all() ->
    eunit:test(
      [
       start_broker()
      ], [verbose]).


start_broker() ->
    {"Start a broker, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, rps:start())
     end}.

%match_2_players() ->
%  {"Match up 2 players",
%   fun() ->
%      {ok, BrokerRef} = rps:start(),
%      Pid = self(),
%      spawn(fun() -> queue_up_worker(Pid, BrokerRef, "John", 5) end),
%      {ok, BOtherP, BCoord} = rps:queue_up(BrokerRef, "Jimbo", 5),
%      {ok, AOtherP, ACoord} = receive
%          Response -> Response
%      end,
%      {"Jimbo", "John", BCoord} =:= {BOtherP, AOtherP, ACoord}
%   end}.
%
%queue_up_worker(Pid, BrokerRef, Name, Rounds) ->
%    Pid ! rps:queue_up(BrokerRef, Name, Rounds).
%
%test_queue_up() ->
%    {ok, BrokerRef} = rps:start(),
%    Pid = self(),
%    spawn(fun() -> queue_up_worker(Pid, BrokerRef, "John", 5) end),
%    B = rps:queue_up(BrokerRef, "Jimbo", 5),
%    A = receive
%        Response -> Response
%    end,
%    io:fwrite("A: ~p~n", [A]),
%    io:fwrite("B: ~p~n", [B])
%    .