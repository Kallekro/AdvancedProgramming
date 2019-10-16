-module(test_rps).
-export([test_all/0]).

%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").


test_all() ->
    eunit:test(
      [
       start_broker(),
       match_players_simple(),
       simple_game(),
       best_of_3(),
       match_players_crowded(),
       simple_stats()
      ], [verbose]).


start_broker() ->
    {"Start a broker, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, rps:start())
     end}.

match_players_simple() ->
  {"Match 2 players up in simple setting",
  fun() ->
    {ok, BrokerRef} = rps:start(),
    Pid = self(),
    spawn(fun() -> queue_up_worker(Pid, BrokerRef, "John", 5) end),
    {ok, Name1, Coordinator} = rps:queue_up(BrokerRef, "Jimbo", 5),
    {ok, Name2, _} = receive
        Response -> Response
    end,
    ?assertMatch({{"Jimbo", Coordinator}, {"John", Coordinator}},
                 {{Name2, Coordinator}, {Name1, Coordinator}})
  end}.

match_players_crowded() ->
  {"Match 2 players in a crowded setting",
  fun() ->
    {ok, BrokerRef} = rps:start(),
    Pid = self(),
    spawn(fun() -> queue_up_worker(Pid, BrokerRef, "Huey", 1) end),
    spawn(fun() -> queue_up_worker(Pid, BrokerRef, "Batman", 5) end),
    spawn(fun() -> queue_up_worker(Pid, BrokerRef, "Dewey", 2) end),
    spawn(fun() -> queue_up_worker(Pid, BrokerRef, "Louie", 3) end),
    {ok, Name1, Coordinator} = rps:queue_up(BrokerRef, "Robin", 5),
    {ok, Name2, _} = receive
        Response -> Response
    end,
    ?assertMatch({{"Robin", Coordinator}, {"Batman", Coordinator}},
                 {{Name2, Coordinator}, {Name1, Coordinator}})
  end
  }.

simple_game() ->
  {"Play a simple game with 1 move each",
  fun() ->
    {ok, BrokerRef} = rps:start(),
    Pid = self(),
    P1 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Batman", 1) end),
    P2 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Robin", 1) end),
    wait_for_matchup(2),
    P1 ! {move, rock},
    P2 ! {move, paper},
    Responses = get_responses("Batman", "Robin"),
    P1 ! done,
    P2 ! done,
    ?assertMatch({{game_over, 0, 1}, {game_over, 1, 0}}, Responses)
  end}.

best_of_3() ->
  {"Play a best of 3 game",
  fun() ->
    {ok, BrokerRef} = rps:start(),
    Pid = self(),
    P1 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Batman", 3) end),
    P2 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Robin", 3) end),
    wait_for_matchup(2),
    P1 ! {move, rock}, P2 ! {move, paper},
    Responses1 = get_responses("Batman", "Robin"),
    P1 ! {move, scissors}, P2 ! {move, paper},
    Responses2 = get_responses("Batman", "Robin"),
    P1 ! {move, rock}, P2 ! {move, rock},
    Responses3 = get_responses("Batman", "Robin"),
    P1 ! {move, paper}, P2 ! {move, rock},
    Responses4 = get_responses("Batman", "Robin"),
    P1 ! done, P2 ! done,
    Responses = [Responses1, Responses2, Responses3, Responses4],
    ?assertMatch([{round_lost, round_won}, {round_won, round_lost},
                  {tie,tie}, {{game_over,2,1}, {game_over,1,2}}], Responses)
  end}.

simple_stats() ->
  {"Stats after 2 games with longest game 3",
  fun() ->
    {ok, BrokerRef} = rps:start(),
    Pid = self(),
    P1 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Batman", 3) end),
    P2 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Robin", 3) end),
    P3 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Huey", 5) end),
    P4 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Dewey", 5) end),
    wait_for_matchup(4),
    % first game
    P1 ! {move, scissors}, P2 ! {move, paper},
    get_responses("Batman", "Robin"),
    P1 ! {move, rock}, P2 ! {move, scissors},
    get_responses("Batman", "Robin"),
    P1 ! done, P2 ! done,
    % second game
    P3 ! {move, rock}, P4 ! {move, paper},
    get_responses("Huey", "Dewey"),
    P3 ! {move, scissors}, P4 ! {move, rock},
    get_responses("Huey", "Dewey"),
    P3 ! {move, paper}, P4 ! {move, scissors},
    get_responses("Huey", "Dewey"),
    P3 ! done, P4 ! done,
    Stats = rps:statistics(BrokerRef),
    ?assertMatch({ok, 3, 0, 0}, Stats)
  end}.

% Helper functions
get_responses(Name1, Name2) ->
  receive
    {FirstName, Res1} ->
      receive
        {_, Res2} ->
          if FirstName =:= Name1 -> {Res1, Res2};
             FirstName =:= Name2 -> {Res2, Res1}
          end
      end
  end.

queue_up_worker(Pid, BrokerRef, Name, Rounds) ->
  Pid ! rps:queue_up(BrokerRef, Name, Rounds).

player_process(Pid, Coordinator, Name) ->
  receive
    {move, Choice} ->
      Pid ! {Name, rps:move(Coordinator, Choice)},
      player_process(Pid, Coordinator, Name);
    done -> exit(normal)
  end.
new_player_process(Pid, BrokerRef, Name, Rounds) ->
  case rps:queue_up(BrokerRef, Name, Rounds) of
    {ok, _, Coordinator} ->
      Pid ! matched_up,
      player_process(Pid, Coordinator, Name);
    OtherResp -> OtherResp
  end.

wait_for_matchup(N) ->
  receive
    matched_up ->
      if (N-1) =:= 0 -> done;
         (N-1) > 0 -> wait_for_matchup(N-1)
      end
  end.

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