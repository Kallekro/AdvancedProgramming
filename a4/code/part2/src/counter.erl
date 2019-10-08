-module(counter).
-export([server/0, main/0]).

inc({_Path, [{Key, Val}| _]}, Counter) ->
  case {Key, list_to_integer(Val)} of
    {"x", IntVal} when IntVal > 0 ->
         Counter ! {"1", IntVal, self()};
    _ -> Counter ! {"1", 1, self()}
  end,
  receive
      S -> {200, "text/plain", S}
  end.

dec({_Path, [{Key, Val}| _]}, Counter) ->
  case {Key, list_to_integer(Val)} of
    {"x", IntVal } when IntVal > 0 -> 
         Counter ! {"0", IntVal, self()};
    _ -> Counter ! {"0", 1, self()}
  end,
  receive
    S -> {200, "text/plain", S}
  end.

count(Count) ->
  receive
    {Type, Val, From} ->
      case Type of
        "1" -> NewVal = Count+Val, From ! (NewVal), count(NewVal);
        "0" -> NewVal = Count-Val, From ! (NewVal), count(NewVal)
      end
  end.        

server() -> 
  Counter = spawn(fun () -> count(0) end),
  {ok, F} = flamingo:start(Counter),
  flamingo:new_route(F, ["/inc_with"], fun inc/2),
  flamingo:new_route(F, ["/dec_with"], fun dec/2),
  F.

try_it(Server) ->
  Me = self(),
  Ref1 = make_ref(),
  Ref2 = make_ref(),
  Ref3 = make_ref(),
  flamingo:request(Server, {"/inc_with", [{"x", "4"}]}, Me, Ref1),
  receive
    {Ref1,Reply} -> io:format("~p~n", [Reply])
  end,
  flamingo:request(Server, {"/dec_with", [{"x", "2"}]}, Me, Ref2),
  receive
    {Ref2, Reply2} -> io:format("~p~n", [Reply2])
  end,
  flamingo:request(Server, {"/inc_with", [{"y", "2"}]}, Me, Ref3),
  receive
    {Ref3, Reply3} -> Reply3
  end.

main() -> 
    Server = server(),
    try_it(Server).