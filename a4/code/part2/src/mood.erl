-module(mood).
-export([server/0, main/0]).

moo({_Path, _}, Tracker) ->
    Tracker ! moo,
    {200, "text/plain", "That's funny"}.

mood({_Path, _}, Tracker) ->
    case request_reply(Tracker, mood) of
        true ->  {200, "Happy"};
        false -> {200, "Sad"}
    end.

request_reply(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        Response -> Response
    end.

moo_tracker(Visited) ->
    receive
        moo -> moo_tracker(true);
        {From, mood} -> From ! Visited, moo_tracker(Visited)
    end.

server() ->
    Tracker = spawn(fun() -> moo_tracker(false) end),
    {ok, F} = flamingo:start(Tracker),
    flamingo:new_route(F, ["/moo"], fun moo/2),
    flamingo:new_route(F, ["/mood"], fun mood/2),
    F.

try_it(Server) ->
    Me = self(),
    Ref1 = make_ref(),
    flamingo:request(Server, {"/mood", []}, Me, Ref1),
    receive
        {Ref1, Reply} -> io:format("~p", [Reply])
    end,
    Ref2 = make_ref(),
    flamingo:request(Server, {"/moo", [Server]}, Me, Ref2),
    receive
        {Ref2, Reply2} -> io:format("~p", [Reply2])
    end,
    Ref3 = make_ref(),
    flamingo:request(Server, {"/mood", []}, Me, Ref3),
    receive
        {Ref3, Reply3} -> Reply3
    end.

main() ->
    Server = server(),
    try_it(Server).

% c(flamingo). c(mood). mood:main().