-module(gml_lexer_test).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
	Document = "graph [ somekey 1 ]",
	{ok, Tokens, _EndLine} = gml_lexer:string(Document),
	io:format("~p\n", [Tokens]),
	?assertMatch([
				  {key, _, "graph", 5},
				  {'[', _},
				  {key, _, "somekey", 7},
				  {integer, _, 1},
				  {']', _}
				 ], Tokens).

