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

nested_test() ->
	GML = "graph
	[
		node [ id 1 ]
		node [ id 2 ]
		node [ id 3 ]
	edge [
		source 2
		target 3
		]
	edge [
		source 3
		target 1
		]
	edge [
		source 2
		target 1
		]
	]",
	% just verify the lexer doesn't croak,
	% we match more sophisticated outputs
	% in gml_test.
	?assertMatch([{key, 1, graph}|_], tokens(GML)). 
