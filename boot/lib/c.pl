c_tokens(Ts) --> c_whitespace, many0(c_token_whitespace, Ts).

c_token_whitespace(T) --> c_token(T), c_whitespace.

c_token(A) --> c_identifier(A), ! ; c_punctuator(A), ! .
c_token(int(N)) --> integer(N), ! .
c_token(char(C)) --> "'", c_char(C), "'", ! .
c_token(string(S)) --> ['"'], many0(c_char, S), ['"'], ! .

c_char(C) --> [92], !, c_special_char(C).
c_char(C) --> char(C).

c_special_char(10) --> "n", ! .
c_special_char(34) --> ['"'].
c_special_char("'") --> "'".

c_identifier(S) --> c_id_head(A), many0(c_id_char, As), { atom_string(S, [A|As]) }.

c_id_head(C) --> char(C), { alpha(C); C="_" }.

c_id_char(C) --> char(C), { alpha(C); C="_"; digit(C, _) }.

c_punctuator(A) --> oneof([
    "[", "]", "(", ")", "{", "}", ".", "->",
    "++", "--", "&", "*", "+", "-", "~", "!", "/", "%",
    "<<", ">>", "<", ">", "<=", ">=", "==", "!=",
    "^", "|", "&&", "||", "?", ":", ";", "...",
    "=",  "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|=",
    ",", "#", "##"], S),
    { atom_string(A, S) }.

c_whitespace --> many0(whitespace), many0((c_comment, many0(whitespace))), ! .

skip_line --> char(10), ! .
skip_line --> char(_), skip_line.

c_comment --> "//", skip_line.
c_comment --> "/*", c_skip_multiline_comment.

c_skip_multiline_comment --> "*/", ! .
c_skip_multiline_comment --> char(C), c_skip_multiline_comment.
