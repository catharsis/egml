-module(gml_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("helpers.hrl").

simple_list_test() ->
	Tokens = tokens("graph [ somekey 1 ]"),
	?assertEqual({ok, {graph, [{somekey, 1}]}}, gml_parser:parse(Tokens)).
