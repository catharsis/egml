-module(gml_lexer_test).
-include_lib("eunit/include/eunit.hrl").

tokens(String) ->
	{ok, Tokens, _EndLine} = gml_lexer:string(String),
	Tokens.

real_number_test() ->
	Tokens = tokens("realsies 4.1E9"),
	?assertMatch([
				  {key, _, "realsies", _},
				  {real, _, 4.1e9}
				 ], Tokens).

simple_test() ->
	Tokens = tokens("graph [ somekey 1 ]"),
	?assertMatch([
				  {key, _, "graph", 5},
				  {'[', _},
				  {key, _, "somekey", 7},
				  {integer, _, 1},
				  {']', _}
				 ], Tokens).

