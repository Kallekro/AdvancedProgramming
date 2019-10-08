-module(mood).
-export([server/0, main/0]).

moo1({_Path, [F]}, G) ->
    io:fwrite("hey"),
    flamingo:new_route(F, ["/moo"], fun moo2/2),
    flamingo:new_route(F, ["/mood"], fun mood2/2),
    moo2({_Path, []}, G).

moo2({_Path, _}, _) ->
    {200, "text/plain",
      "That's funny"}.

mood1({_Path, _}, _) ->
    {200, "text/plain",
      "Sad"}.

mood2({_Path, _}, _) ->
    {200, "text/plain",
      "Happy!"}.

server() ->
    {ok, F} = flamingo:start("The Flamingo Server"),
    flamingo:new_route(F, ["/moo"], fun moo1/2),
    flamingo:new_route(F, ["/mood"], fun mood1/2),
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