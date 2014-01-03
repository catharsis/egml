-module(gml).
-export([from_string/1, from_string/2, to_digraph/1, from_file/1, from_file/2]).
-export([node_properties/2, edge_properties/2]).

-define(LEXER, gml_lexer).
-define(PARSER, gml_parser).
-type gml_graph() :: {graph, gml_list()}.
-type gml_list() :: [{Key::atom(), Val::gml_value()}].
-type gml_value() :: number() | string() | gml_list().
-type gml_options() :: [gml_option()].
-type gml_option() :: verify_edges.

-export_type([gml_graph/0]).

-spec from_string(GML::string()) -> {ok, gml_graph()} | {error, term()}.
from_string(GML) ->
	from_string(GML, []).

-spec from_string(GML::string(), Options::gml_options()) -> {ok, gml_graph()} | {error, term()}.
from_string(GML, Options) ->
	{ok, Tokens, _EndLine} = ?LEXER:string(GML),
	case ?PARSER:parse(Tokens) of
		{ok, Graph} ->
			{ok, verify_graph(Graph, Options)};
		Other ->
			Other
	end.

-spec from_file(FileName::string()) -> {ok, gml_graph()} | {error, term()}.
from_file(FileName) ->
	from_file(FileName, []).

-spec from_file(FileName::string(), Options::gml_options()) -> {ok, gml_graph()} | {error, term()}.
from_file(FileName, Options) ->
	{ok, File} = file:open(FileName, [read]),
	Tokens = loop(File, []),
	file:close(File),
	case ?PARSER:parse(Tokens) of
		{ok, Graph} ->
			{ok, verify_graph(Graph, Options)};
		Other ->
			Other
	end.

-spec node_properties(Graph::gml_graph(), NodePropertyKeys::[atom()]) -> [NodePropertyValues::tuple()].
node_properties({graph, Elements}, Keys) ->
	element_properties(node, Elements, Keys).

-spec edge_properties(Graph::gml_graph(), EdgePropertyKeys::[atom()]) -> [EdgePropertyValues::tuple()].
edge_properties({graph, Elements}, Keys) ->
	element_properties(edge, Elements, Keys).

element_properties(ElementName, Elements, Keys) ->
	lists:foldl(fun (Element, Acc) ->
						case Element of
							{ElementName, Properties} ->
								FilteredProperties = lists:map(fun(Key) ->
																	   proplists:get_value(Key, Properties)
															   end, Keys),
								[list_to_tuple(FilteredProperties) | Acc];
							_ ->
								Acc
						end
				end, [], Elements).

loop(File, Acc) ->
	case io:request(File, {get_until, prompt, ?LEXER, token, [1]}) of
		{eof, _} ->
			Acc;
		{ok, Token, _} ->
			loop(File, Acc ++ [Token]);
		{error, token} ->
			exit(lexer_error)
	end.




-spec to_digraph(Graph::gml_graph()) -> {ok, digraph()} | {error, Reason::term()}.
to_digraph({graph, Graph}) ->
	to_digraph(Graph, digraph:new()).

to_digraph([], Digraph) ->
	{ok, Digraph};
to_digraph([{node, Node} | T], Digraph) ->
	%add vertex to digraph, using the Node itself as a label
	% in order to preserve the data
	digraph:add_vertex(Digraph,
									proplists:get_value(id, Node),
									Node),
	to_digraph(T, Digraph);
to_digraph([{edge, Edge} | T], Digraph) ->
	%add edge to digraph
	digraph:add_edge(Digraph,
					 proplists:get_value(source, Edge),
					 proplists:get_value(target, Edge),
					Edge),
	to_digraph(T, Digraph);
to_digraph([ _Other | T], Digraph) ->
	to_digraph(T, Digraph).

verify_graph(Graph, []) ->
	Graph; % success,
verify_graph(Graph, [verify_edges | Options]) ->
	EdgeProps = edge_properties(Graph, [source, target]),
	case {lists:keymember(undefined, 1, EdgeProps), lists:keymember(undefined, 2, EdgeProps)} of
		{_, true} ->
			error(missing_edge_target);
		{true, _} ->
			error(missing_edge_source);
		{false, false} ->
			ok
	end,
	verify_graph(Graph, Options);
verify_graph(Graph, [_Option | Options]) ->
	verify_graph(Graph, Options).


