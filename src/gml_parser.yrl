Nonterminals
kv kvs list value.

Terminals '[' ']'
string integer key real.

Rootsymbol kv.

kv -> key value : {unwrap('$1'), '$2'}. 

list -> '[' ']' : nil.
list -> '[' kvs ']' : '$2'.

kvs -> kv : ['$1'].
kvs -> kv kvs : ['$1'] ++ '$2'.

value -> list : '$1'.
value -> real : unwrap('$1').
value -> integer : unwrap('$1').
value -> string : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.

