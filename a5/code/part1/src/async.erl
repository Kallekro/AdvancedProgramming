-module(async).

-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).

action(Fun, Arg) ->
    Aid = self(),
    spawn(fun() -> worker(Aid, Fun, Arg) end),
    action_loop(none, false).

wait_action_loop(From) ->
    receive
        {done, Res} -> From ! Res;
        _ -> wait_action_loop(From)
    end.

action_loop(Res, ResFlag) ->
    receive
        {done, NewRes} ->
            action_loop(NewRes, true);
        {From, poll} ->
            if ResFlag =:= true -> From ! Res;
               ResFlag =:= false ->
                    From ! nothing,
                    action_loop(Res, ResFlag)
            end;
        {From, wait} ->
            if ResFlag =:= false -> wait_action_loop(From);
               ResFlag =:= true -> From ! Res
            end;
        _ -> action_loop(Res, ResFlag)
    end.

worker(Aid, Fun, Arg) ->
    Res = try
        {ok, Fun(Arg)}
    catch
        E -> {exception, E}
    end,
    Aid ! {done, Res}.

new(Fun, Arg) ->
    spawn(fun() -> action(Fun, Arg) end).

wait(Aid) ->
    Aid ! {self(), wait},
    receive
        {ok, Res} -> Res;
        {exception, E} -> throw(E)
    end.

poll(Aid) ->
    Aid ! {self(), poll},
    receive
        nothing -> nothing;
        {ok, Res} -> {ok, Res};
        {exception, E} -> {exception, E}
    end.


% c(async). async:test1().

%% Optional functions, recommended

wait_catch(Aid) ->
    try wait(Aid)
    catch
       E -> {exception, E}
    end.

wait_any([H|T]) ->
    case poll(H) of
        nothing -> wait_any(T ++ [H]);
        {ok, Res} -> {H, Res};
        {exception, E} -> throw(E)
    end.
