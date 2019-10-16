-module(rps).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).

coordinator_draining(PlayerNotified) ->
    receive
        {From, move, _} ->
            From ! server_stopping,
            if PlayerNotified =:= none -> coordinator_draining(From);
               PlayerNotified /=  none ->
                   if From =:= PlayerNotified -> coordinator_draining(PlayerNotified);
                      From /= PlayerNotified -> pass
                    end
            end;
        _ -> coordinator_draining(PlayerNotified)
    end.

coordinator(GameManager, FirstMove, RoundsToWin, {{Pid1, Wins1}, {Pid2, Wins2}}, RoundCount) ->
  receive
    {From, move, Choice} ->
      case FirstMove of
        none -> coordinator(GameManager, {From, Choice}, RoundsToWin,  {{Pid1, Wins1}, {Pid2, Wins2}}, RoundCount);
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
            tie -> {Wins1, Wins2}
          end,
          if (NewWins1 > RoundsToWin) or (NewWins2 > RoundsToWin) ->
                Pid1 ! {game_over, NewWins1, NewWins2},
                Pid2 ! {game_over, NewWins2, NewWins1},
                GameManager ! {coord_done, self(), RoundCount+1},
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
          coordinator(GameManager, none, RoundsToWin, {{Pid1, NewWins1}, {Pid2, NewWins2}}, RoundCount+1)
      end;
    drain -> coordinator_draining(none);
    _ -> coordinator(GameManager, FirstMove, RoundsToWin, {{Pid1, Wins1}, {Pid2, Wins2}}, RoundCount)
  end.

eval_rps(C1, C2) when C1 =:= C2 -> tie;
eval_rps(rock, paper) -> choice2;
eval_rps(rock, scissors) -> choice1;
eval_rps(paper, scissors) -> choice2;
eval_rps(paper, rock) -> choice1;
eval_rps(scissors, rock) -> choice2;
eval_rps(scissors, paper) -> choice1.

pair_up(_, []) -> false;
pair_up(P, [H|T]) ->
    case {P, H} of
        {{_, _, Rounds}, {From2, Name2, Rounds}} ->
            {true, {From2, Name2, Rounds}};
        _ -> pair_up(P, T)
    end.

find_matches([]) -> [];
find_matches([P1|T]) ->
    case pair_up(P1, T) of
        {true, P2} ->
            TmpT = remove_from_list(T, P2),
            [{P1, P2}|find_matches(TmpT)];
        false -> find_matches(T)
    end.

remove_from_list([], _) -> [];
remove_from_list([H|T], P) ->
    if P =:= H -> T;
       P /= H -> [H|remove_from_list(T, P)]
    end.

matchup_pairs(Queue, [], NewCoordinators) -> {Queue, NewCoordinators};
matchup_pairs(Queue, [H|T], NewCoordinators) ->
    {{Pid1, Name1, Rounds}, {Pid2, Name2, Rounds}} = H,
    GameManager = self(),
    Coordinator = spawn(fun() -> coordinator(GameManager, none, Rounds div 2,
                                             {{Pid1, 0}, {Pid2, 0}}, 0) end),
    Pid1 ! {ok, Name2, Coordinator},
    Pid2 ! {ok, Name1, Coordinator},
    TmpQueue = remove_from_list(remove_from_list(Queue,
                {Pid1, Name1, Rounds}), {Pid2, Name2, Rounds}),
    matchup_pairs(TmpQueue, T, [Coordinator|NewCoordinators]).

drain_coordinators([]) -> true;
drain_coordinators([C|CT]) ->
    C ! drain,
    drain_coordinators(CT).

drain_queue([]) -> true;
drain_queue([Q|T]) ->
    Q ! server_stopping,
    drain_queue(T).

game_manager(Queue, LongestGame, Coordinators) ->
    receive
        {queue_up, From, Name, Rounds} ->
            TmpQueue1 = Queue ++ [{From, Name, Rounds}],
            NewMatches = find_matches(TmpQueue1),
            {TmpQueue2, NewCoordinators} = matchup_pairs(TmpQueue1, NewMatches, []),
            game_manager(TmpQueue2, LongestGame, Coordinators ++ NewCoordinators);
        {coord_done, CoordinatorPid, Rounds} ->
            TmpCoordinators = remove_from_list(Coordinators, CoordinatorPid),
            if Rounds > LongestGame ->
                game_manager(Queue, Rounds, TmpCoordinators);
               Rounds < LongestGame ->
                game_manager(Queue, LongestGame, TmpCoordinators)
            end;
        {stats, From} ->
            From ! {ok, LongestGame, length(Queue), length(Coordinators)},
            game_manager(Queue, LongestGame, Coordinators);
        {drain, Pid, Msg} ->
            drain_queue(Queue),
            drain_coordinators(Coordinators),
            if Pid =:= none -> pass;
               Pid /= none -> Pid ! Msg
            end;
        _ -> game_manager(Queue, LongestGame, Coordinators)
    end.

broker(GameManager) ->
    receive
        Request ->
            GameManager ! Request,
            broker(GameManager)
    end.

start() ->
    GameManager = spawn(fun() -> game_manager([], 0, []) end),
    BrokerRef = spawn(fun() -> broker(GameManager) end),
    {ok, BrokerRef}.

queue_up(BrokerRef, Name, Rounds) ->
    BrokerRef ! {queue_up, self(), Name, Rounds},
    receive
        Response -> Response
    end.

move(Coordinator, Choice) ->
    Coordinator ! {self(), move, Choice},
    receive
        Response -> Response
    end.

statistics(BrokerRef) ->
    BrokerRef ! {stats, self()},
    receive
        Stats -> Stats
    end.

drain(BrokerRef, Pid, Msg) ->
    BrokerRef ! {drain, Pid, Msg}.




% Tests
%queue_up_worker(Pid, BrokerRef, Name, Rounds) ->
%    Pid ! rps:queue_up(BrokerRef, Name, Rounds).
%
%
%player_process(Pid, Coordinator, Name) ->
%    receive
%        {move, Choice} ->
%            Pid ! {Name, move(Coordinator, Choice)},
%            player_process(Pid, Coordinator, Name);
%        done -> exit(normal)
%    end.
%
%new_player_process(Pid, BrokerRef, Name, Rounds) ->
%    case rps:queue_up(BrokerRef, Name, Rounds) of
%        {ok, _, Coordinator} ->
%            Pid ! matched_up,
%            player_process(Pid, Coordinator, Name);
%        OtherResp -> OtherResp
%    end.
%
%wait_for_matchup() ->
%    receive
%        matched_up -> done
%    end.
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
%    io:fwrite("B: ~p~n", [B]).
%
%test_move1() ->
%    {ok, BrokerRef} = rps:start(),
%    Pid = self(),
%    P1 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Preben", 1) end),
%    P2 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Jeppe", 1) end),
%    wait_for_matchup(), wait_for_matchup(),
%    P1 ! {move, rock},
%    P2 ! {move, paper},
%    Mv1 = receive
%        Resp1 -> Resp1
%    end,
%    Mv2 = receive
%        Resp2 -> Resp2
%    end,
%    io:fwrite("~p~n", [Mv1]),
%    io:fwrite("~p~n", [Mv2]),
%    P1 ! done,
%    P2 ! done
%    .
%
%test_move2() ->
%    {ok, BrokerRef} = rps:start(),
%    Pid = self(),
%    P1 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Preben", 5) end),
%    P2 = spawn(fun() -> new_player_process(Pid, BrokerRef, "Jeppe", 5) end),
%    wait_for_matchup(), wait_for_matchup(),
%    P1 ! {move, rock},
%    P2 ! {move, paper},
%    Mv1_1 = receive
%        Resp1_1 -> Resp1_1
%    end,
%    Mv2_1 = receive
%        Resp2_1 -> Resp2_1
%    end,
%
%    P1 ! {move, rock},
%    P2 ! {move, paper},
%    Mv1_2 = receive
%        Resp1_2 -> Resp1_2
%    end,
%    Mv2_2 = receive
%        Resp2_2 -> Resp2_2
%    end,
%    P1 ! {move, rock},
%    P2 ! {move, paper},
%    Mv1_3 = receive
%        Resp1_3 -> Resp1_3
%    end,
%    Mv2_3 = receive
%        Resp2_3 -> Resp2_3
%    end,
%
%    io:fwrite("~p~n", [Mv1_1]),
%    io:fwrite("~p~n", [Mv2_1]),
%    io:fwrite("~p~n", [Mv1_2]),
%    io:fwrite("~p~n", [Mv2_2]),
%    io:fwrite("~p~n", [Mv1_3]),
%    io:fwrite("~p~n", [Mv2_3]),
%    P1 ! done,
%    P2 ! done,
%
%    Stats = stats(BrokerRef),
%    io:fwrite("~p~n", [Stats])
%    .
%
%test_drain() ->
%
%    {ok, BrokerRef} = rps:start(),
%    Pid = self(),
%    spawn(fun() -> new_player_process(Pid, BrokerRef, "Preben", 5) end),
%    spawn(fun() -> new_player_process(Pid, BrokerRef, "Jeppe", 5) end),
%    wait_for_matchup(), wait_for_matchup(),
%    spawn(fun() -> new_player_process(Pid, BrokerRef, "James", 3) end),
%
%    BrokerRef ! {drain, self(), "Don Draining"},
%    io:fwrite("Waiting for drain..~n"),
%    receive
%        DrainResp -> io:fwrite("~p~n", [DrainResp])
%    end.