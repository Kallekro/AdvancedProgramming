-module(flamingo).
-export([start/1, new_route/3, request/4, drop_route/2]).

startswith([], _) -> true;
startswith([_|_], []) -> true;
startswith([H1|T1], [H2|T2]) -> (H1 =:= H2) and startswith(T1, T2).

lookup([], _, {_,Res}) -> Res;
lookup([{Key, Val}|T], X, {ResK, ResV}) ->
    case startswith(X, Key) of
        true ->
            if length(Key) > length(ResK) -> lookup(T, X, {Key, Val});
               length(Key) =< length(ResK) -> lookup(T, X, {ResK, ResV})
            end;
        false -> lookup(T, X, {ResK, ResV})
    end.

create_routes([], _Action) -> [];
create_routes([H|T], _Action) -> [{H, _Action}|create_routes(T, _Action)].

server_loop(_Global, Routes) ->
  receive
    {new_route, _Prefixes, _Action} ->
      server_loop(_Global, create_routes(_Prefixes, _Action) ++ Routes);
    {request, {Path, Args}, _From, _Ref} ->
      case lookup(Routes, Path, {"", none}) of
        none -> _From ! {_Ref, {404, "text/plain", "No matching routes."}};
        Action -> _From ! {_Ref, Action({Path, Args}, _Global)}
      end,
      server_loop(_Global, Routes)
  end.

start(_Global) -> {ok, spawn(fun() -> server_loop(_Global, []) end)}.

request(_Flamingo, _Request, _From, _Ref) ->
  _Flamingo ! {request, _Request, _From, _Ref}.

new_route(_Flamingo, _Prefixes, _Action) ->
  _Flamingo ! {new_route, _Prefixes, _Action}.

drop_route(_Flamingo, _Id) ->
  not_implemented.
