-module(hello).
-export([server/0, main/0]).

hello({_Path, []}, _) ->
    {200, "text/plain",
     "Hello my dear friend"}.

goodbye({_Path, []}, _) ->
    {200, "text/plain",
     "Sad to see you go already."}.

server() ->
    {ok, F} = flamingo:start("The Flamingo Server"),
    flamingo:new_route(F, ["/hello"], fun hello/2),
    flamingo:new_route(F, ["/goodbye"], fun goodbye/2),
    F.

try_it(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/hello", []}, Me, Ref),
    receive
        {Ref, Reply} -> io:format("~p", [Reply])
    end,
    flamingo:request(Server, {"/goodbye", []}, Me, Ref),
    receive
        {Ref, Reply2} -> Reply2
    end.

% c(flamingo). c(hello). hello:main().

main() ->
    Server = server(),
    try_it(Server).