-module(gml_test).
-include_lib("eunit/include/eunit.hrl").

% testdata paths are ../test/testdata because rebar runs eunit tests
% in <project root>/.eunit/ by default.
-define(testdata_simple, "../test/testdata/simple.gml").
%% Testdata is
%% ./testdata/simple.gml:
%%		Represents a planar, directed graph
%%		of three nodes with three edges.
%%		All nodes and all edges are labeled with a
%%		short description.
%%
%%		The edges are as follows:
%%			1 -> 2
%%			2 -> 3
%%			3 -> 1
from_file_enoent_test() ->
	?assertEqual({error, enoent}, gml:from_file("/not/a/file", [])).

from_file_test_() ->
	{ok, Graph} = gml:from_file(?testdata_simple, [verify_edges]),
	NodeProperties = lists:sort(gml:node_properties(Graph, [id, label])),
	EdgeProperties = lists:sort(gml:edge_properties(Graph, [source, target, label])),
	[
	 ?_assertEqual([{1, "Node 1"}, {2, "Node 2"}, {3, "Node 3"}], NodeProperties),
	 ?_assertEqual([{1, 2, "Edge from node 1 to node 2"},
					{2, 3, "Edge from node 2 to node 3"},
					{3, 1, "Edge from node 3 to node 1"}],
				   EdgeProperties),
	 ?_assertEqual({graph,
					[
					 {comment, "This is a sample graph"},
					 {directed, 1},
					 {isPlanar, 1},
					 {node, [{id, 1}, {label, "Node 1"}]},
					 {node, [{id, 2}, {label, "Node 2"}]},
					 {node, [{id, 3}, {label, "Node 3"}]},
					 {edge, [{source, 1}, {target, 2}, {label, "Edge from node 1 to node 2"}]},
					 {edge, [{source, 2}, {target, 3}, {label, "Edge from node 2 to node 3"}]},
					 {edge, [{source, 3}, {target, 1}, {label, "Edge from node 3 to node 1"}]}
					  ]
				   }, Graph),
	 ?_assertError(missing_edge_target, gml:from_string("graph [edge [ source 1]]", [verify_edges])),
	 ?_assertError(missing_edge_source, gml:from_string("graph [edge [ target 1]]", [verify_edges]))
	].

from_string_test_() ->
	GML = "graph [ node [ id 1 ]]",
	{ok, Graph} = gml:from_string(GML),
	[
	 ?_assertMatch({graph, _}, Graph),
	 ?_assertMatch({error, _}, gml:from_string("graph [ node [ ]"))
	].

%% GML represents a graph like
%%  1<--2-->3
%%  ^-------'
%%
%%  Verify that the resulting digraph reflects this.
to_digraph_test_() ->
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
	{ok, Graph} = gml:from_string(GML),
	{ok, Digraph} = gml:to_digraph(Graph),
	[
	 ?_assertEqual(3, digraph:no_edges(Digraph)),
	 ?_assertEqual(3, digraph:no_vertices(Digraph)),
	 ?_assertEqual(2, length(digraph:edges(Digraph, 3))),
	 ?_assertEqual(2, length(digraph:edges(Digraph, 2))),
	 ?_assertEqual(2, length(digraph:edges(Digraph, 1))),
	 ?_assertEqual(2, digraph:in_degree(Digraph, 1)),
	 ?_assertEqual(0, digraph:out_degree(Digraph, 1)),
	 ?_assertEqual(0, digraph:in_degree(Digraph, 2)),
	 ?_assertEqual(2, digraph:out_degree(Digraph, 2)),
	 ?_assertEqual(1, digraph:in_degree(Digraph, 3)),
	 ?_assertEqual(1, digraph:out_degree(Digraph, 3))
	].
