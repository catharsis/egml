Definitions.

SIGN = (\+|-|)
D = [0-9]
INSTRING = ([\x00-\x21\x23-\x25\x27-\x7F]|&.+;)*
STRING = "{INSTRING}"
WS = [\t\s\n]
KEY = [a-zA-Z][a-zA-Z0-9]*
INTEGER = {SIGN}{D}+
MANTISSA = (E{SIGN}{D}+)?
REAL = {SIGN}{D}*\.{D}*{MANTISSA}
COMMENT = #.*
Rules.

{STRING} :
	{token, {string, TokenLine, strip_quots(TokenChars, TokenLen)}}.
{INTEGER}		:
	{token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{KEY}			:
	{token, {key, TokenLine, list_to_atom(TokenChars)}}.
{REAL} :
	{token, {real, TokenLine, list_to_float(TokenChars)}}.
[\[\]] :
	{token, {list_to_atom(TokenChars), TokenLine}}.
{WS}+ :
	skip_token.
{COMMENT} :
	skip_token.

Erlang code.
strip_quots(S, L) ->
	string:substr(S, 2, L-2).

