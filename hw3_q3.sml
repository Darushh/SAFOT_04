datatype Atom = NIL | SYMBOL of string (*the requested datatypes*)
datatype SExp = ATOM of Atom | CONS of (SExp * SExp)

local
    fun parse_read [] = raise Empty 
      | parse_read ("(" :: tokens) = parse_list tokens (*the start of a list*)
      | parse_read (")" :: tokens) = raise Fail "Unexpected closing parenthesis"
      | parse_read ("NIL" :: tokens) = (ATOM NIL, tokens)
      | parse_read (s :: tokens) = (ATOM (SYMBOL s), tokens)

    and parse_list [] = raise Fail "Unbalanced parenthesis: missing )"
      | parse_list (")" :: tokens) = (ATOM NIL, tokens)
      | parse_list tokens =
        let
            val (first_sexp, rest1) = parse_read tokens (*parsing first item with read*)
            val (rest_sexp, rest2) = parse_list rest1 (*parsing remaining as other list*)
        in
            (CONS (first_sexp, rest_sexp), rest2)
        end
in
    fun parse tokens =
        let
            val (result, rest) = parse_read tokens
        in
            result
        end
end