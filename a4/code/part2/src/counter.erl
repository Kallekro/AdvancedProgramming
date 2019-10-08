-module(counter).
-export([server/0, main/0]).

get_val(_, []) -> 1;
get_val(Key, [{K,V}|Reqs]) ->
  case {K,list_to_integer(V)} of
    {Key, IntVal} when IntVal > 0 -> IntVal;
    {Key, _} -> 1;
    _ -> get_val(Key, Reqs)
  end. 

inc({_Path, Reqs}, Counter) ->
  Counter ! {inc, get_val("x", Reqs), self()},
  receive
      S -> {200, "text/plain", S}
  end.

dec({_Path, Reqs}, Counter) ->
  Counter ! {dec, get_val("x", Reqs), self()},
  receive
    S -> {200, "text/plain", S}
  end.

count(Count) ->
  receive
    {inc, Val, From} -> NewVal=Count+Val;
    {dec, Val, From} -> NewVal=Count-Val 
  end,
  From ! NewVal,
  count(NewVal).

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
  Ref4 = make_ref(),
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
    {Ref3, Reply3} -> io:format("~p~n", [Reply3])
  end,
  flamingo:request(Server, {"/inc_with", []}, Me, Ref4),
  receive
    {Ref4, Reply4} -> Reply4
  end.

main() -> 
    Server = server(),
    try_it(Server).