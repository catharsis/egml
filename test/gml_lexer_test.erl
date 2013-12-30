-module(gml_lexer_test).
-include_lib("eunit/include/eunit.hrl").
-include("helpers.hrl").
real_number_test() ->
	Tokens = tokens("realsies 4.1E9"),
	?assertMatch([
				  {key, _, realsies},
				  {real, _, 4.1e9}
				 ], Tokens).

simple_test() ->
	Tokens = tokens("graph [ somekey 1 ]"),
	?assertMatch([
				  {key, _, graph},
				  {'[', _},
				  {key, _, somekey},
				  {integer, _, 1},
				  {']', _}
				 ], Tokens).

