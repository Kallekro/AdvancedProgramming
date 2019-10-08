-module(greetings).
-export([server/0, try_it/1]).

greeter({_Path, [{"name", Name} | _ ]}, Server) ->
    {200, "text/plain",
     lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Server])}.


fareweller({_Path, [{"name", Name} | _ ]}, _) ->
    {200, "text/plain",
     lists:concat(["Farewell ", Name, "\n",
                   "You are banished to eternity in hell."])}.

server() ->
    {ok, F} = flamingo:start("The Flamingo Server"),
    flamingo:new_route(F, ["/hello"], fun greeter/2),
    flamingo:new_route(F, ["/hello"], fun fareweller/2),
    F.

try_it(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/hello", [{"name", "Student"}]},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.

% c(flamingo). c(greetings). greetings:server().