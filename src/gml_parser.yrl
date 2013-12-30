Nonterminals
list value.

Terminals '[' ']'
string integer key real.

Rootsymbol list.

list -> key value : {unwrap('$1'), '$2'}. 


value -> '[' list ']' : ['$2'].
value -> real : unwrap('$1').
value -> integer : unwrap('$1').
value -> string : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.
