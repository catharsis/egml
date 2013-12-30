tokens(String) ->
	{ok, Tokens, _EndLine} = gml_lexer:string(String),
	Tokens.

