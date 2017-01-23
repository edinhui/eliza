% eliza leex file

Definitions.
D = [0-9]
L = [A-Za-z]
S = [?*'()@,.-]
SP = (:)
W = (\s|\n|\t)

Rules.
initial : {token,{initial, TokenLine, list_to_atom(TokenChars)}}.
final   : {token,{final, TokenLine, list_to_atom(TokenChars)}}.
quit    : {token,{quit, TokenLine, list_to_atom(TokenChars)}}.
pre     : {token,{pre, TokenLine, list_to_atom(TokenChars)}}.
post    : {token,{post, TokenLine, list_to_atom(TokenChars)}}.
synon   : {token,{post, TokenLine, list_to_atom(TokenChars)}}.
key     : {token,{key, TokenLine, list_to_atom(TokenChars)}}.
decomp  : {token,{decomp, TokenLine, list_to_atom(TokenChars)}}.
reasemb : {token,{reasemb, TokenLine, list_to_atom(TokenChars)}}.
{SP}    : {token,{split, TokenLine, split}}.
{S}*{D}+{S}*  : {token,{word, TokenLine, TokenChars}}.
{S}*{L}+{S}*  : {token,{word, TokenLine, TokenChars}}.
{S}*    :  {token,{word, TokenLine, TokenChars}}.
(\$)*   :  {token,{word, TokenLine, TokenChars}}.  
{W}+    : skip_token.

Erlang code.


