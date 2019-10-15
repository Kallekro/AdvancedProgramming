-module(rps).
-export([start/0, queue_up/3, move/2, stats/1, drain/3, test_queue_up/0, test_move1/0, test_move2/0]).

coordinator(BrokerRef, FirstMove, RoundsToWin, {{Pid1, Wins1}, {Pid2, Wins2}}, RoundCount) ->
  receive
    {From, move, Choice} ->
      case FirstMove of
        none -> coordinator(BrokerRef,{From, Choice}, RoundsToWin,  {{Pid1, Wins1}, {Pid2, Wins2}}, RoundCount);
        {From1, Choice1} ->
          R = eval_rps(Choice1, Choice),
          {NewWins1, NewWins2} = case R of
            choice1 ->
              if From1 =:= Pid1 -> {Wins1+1, Wins2};
                 From1 /=  Pid1 -> {Wins1, Wins2+1}
              end;
            choice2 ->
              if From1 =:= Pid1 -> {Wins1, Wins2+1};
                 From1 /=  Pid1 -> {Wins1+1, Wins2}
              end;
            tie -> From1 ! tie, From ! tie, {Wins1, Wins2}
          end,
          if (NewWins1 > RoundsToWin) or (NewWins2 > RoundsToWin) ->
                Pid1 ! {game_over, NewWins1, NewWins2},
                Pid2 ! {game_over, NewWins2, NewWins1},
                BrokerRef ! {coord_done, self(), RoundCount+1},
                exit(normal);
             Wins1 < NewWins1 ->
                 Pid1 ! round_won,
                 Pid2 ! round_lost;
             Wins2 < NewWins2 ->
                 Pid1 ! round_lost,
                 Pid2 ! round_won;
             (Wins1 =:= NewWins1) and (Wins2 =:= NewWins2) ->
                 Pid1 ! tie,
                 Pid2 ! tie
          end,
          coordinator(BrokerRef, none, RoundsToWin, {{Pid1, NewWins1}, {Pid2, NewWins2}}, RoundCount+1)
      end
  end.

eval_rps(C, C) -> tie;
eval_rps(rock, paper) -> choice2;
eval_rps(rock, scissors) -> choice1;
eval_rps(paper, scissors) -> choice2;
eval_rps(paper, rock) -> choice1;
eval_rps(scissors, rock) -> choice2;
eval_rps(scissors, paper) -> choice1.

match_up(_, _, []) -> false;
match_up(BrokerRef, P, [H|T]) ->
    case {P, H} of
        {{From1, Name1, Rounds}, {From2, Name2, Rounds}} ->
            Coordinator = spawn(fun() -> coordinator(BrokerRef,none, Rounds div 2, {{From1, 0}, {From2, 0}}, 0) end),
            From1 ! {ok, Name2, Coordinator},
            From2 ! {ok, Name1, Coordinator},
            {true, H, Coordinator};
        _ -> match_up(BrokerRef, P, T)
    end.

find_matches(BrokerRef, [], Res, Coordinators) -> BrokerRef ! {new_queue, Res, Coordinators};
find_matches(BrokerRef, [P1|T], Res, Coordinators) ->
    case match_up(BrokerRef, P1, T) of
        {true, P2, Coordinator} ->
            TmpT = remove_from_list(T, P2),
            find_matches(BrokerRef, TmpT, Res, [Coordinator|Coordinators]);
        false -> find_matches(BrokerRef, T, [P1|Res], Coordinators)
    end.

remove_from_list([], _) -> [];
remove_from_list([H|T], P) ->
    if P =:= H -> T;
       P /= H -> [H|remove_from_list(T, P)]
    end.

broker(Queue, MatchFlag, LongestGame, Coordinators) ->
    if MatchFlag =:= true ->
            BrokerRef = self(),
            io:fwrite("~p~n", [Queue]),
            spawn(fun() -> find_matches(BrokerRef, Queue, [], []) end);
       MatchFlag =:= false -> MatchFlag
    end,
    receive
        {queue, From, Name, Rounds} ->
            TmpQueue = Queue ++ [{From, Name, Rounds}],
            broker(TmpQueue, MatchFlag, LongestGame, Coordinators);
        {new_queue, NewQueue, Coordinators} -> broker(NewQueue, true, LongestGame, Coordinators);
        {coord_done, CoordinatorPid, Rounds} ->
            Coordinators = remove_from_list(Coordinators, CoordinatorPid),
            if Rounds > LongestGame ->
                broker(Queue, MatchFlag, Rounds, Coordinators);
               Rounds < LongestGame ->
                broker(Queue, MatchFlag, LongestGame, Coordinators)
            end;
        {stats, From} ->
            From ! {ok, LongestGame, length(Queue), length(Coordinators)},
            broker(Queue, MatchFlag, LongestGame, Coordinators)
    end.

start() -> {ok, spawn(fun() -> broker([], true, 0 ,[]) end)}.

queue_up(BrokerRef, Name, Rounds) ->
    BrokerRef ! {queue, self(), Name, Rounds},
    receive
       {ok, OtherPlayer, Coordinator} -> {ok, OtherPlayer, Coordinator}
    end.

move(Coordinator, Choice) ->
    Coordinator ! {self(), move, Choice},
    receive
        Response -> Response
    end.

stats(BrokerRef) ->
    BrokerRef ! {stats, self()},
    receive
        Stats -> Stats
    end.

drain(_, _, _) ->  nope.




% Tests
queue_up_worker(Pid, BrokerRef, Name, Rounds) ->
    Pid ! rps:queue_up(BrokerRef, Name, Rounds).


player_process(Pid, Coordinator, Name) ->
    receive
        {move, Choice} ->
            Pid ! {Name, move(Coordinator, Choice)},
            player_process(Pid, Coordinator, Name);
        done -> exit(normal)
    end.

new_player_process(Pid, BrokerRef, Name, Rounds) ->
    {ok, _, Coordinator} = rps:queue_up(BrokerRef, Name, Rounds),
    player_process(Pid, Coordinator, Name).

test_queue_up() ->
    {ok, BrokerRef} = rps:start(),
    Pid = self(),
    spawn(fun() -> queue_up_worker(Pid, BrokerRef, "John", 5) end),
    B = rps:queue_up(BrokerRef, "Jimbo", 5),
    A = receive
        Response -> Response
    end,
    io:fwrite("A: ~p~n", [A]),
    io:fwrite("B: ~p~n", [B])
    .

test_move1() ->
    {ok, BrokerRef} = rps:start(),
    Pid = self(),
    P1 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Preben", 1) end),
    P2 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Jeppe", 1) end),
    P1 ! {move, rock},
    P2 ! {move, paper},
    Mv1 = receive
        Resp1 -> Resp1
    end,
    Mv2 = receive
        Resp2 -> Resp2
    end,
    io:fwrite("~p~n", [Mv1]),
    io:fwrite("~p~n", [Mv2]),
    P1 ! done,
    P2 ! done
    .

test_move2() ->
    {ok, BrokerRef} = rps:start(),
    Pid = self(),
    P1 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Preben", 5) end),
    P2 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Jeppe", 5) end),
    P1 ! {move, rock},
    P2 ! {move, paper},
    Mv1_1 = receive
        Resp1_1 -> Resp1_1
    end,
    Mv2_1 = receive
        Resp2_1 -> Resp2_1
    end,

    P1 ! {move, rock},
    P2 ! {move, paper},
    Mv1_2 = receive
        Resp1_2 -> Resp1_2
    end,
    Mv2_2 = receive
        Resp2_2 -> Resp2_2
    end,
    P1 ! {move, rock},
    P2 ! {move, paper},
    Mv1_3 = receive
        Resp1_3 -> Resp1_3
    end,
    Mv2_3 = receive
        Resp2_3 -> Resp2_3
    end,

    io:fwrite("~p~n", [Mv1_1]),
    io:fwrite("~p~n", [Mv2_1]),
    io:fwrite("~p~n", [Mv1_2]),
    io:fwrite("~p~n", [Mv2_2]),
    io:fwrite("~p~n", [Mv1_3]),
    io:fwrite("~p~n", [Mv2_3]),
    P1 ! done,
    P2 ! done,

    Stats = stats(BrokerRef),
    io:fwrite("~p~n", [Stats])
    .