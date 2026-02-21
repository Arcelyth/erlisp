-module(erlisp).
-export([main/0]).
-type chan() :: {file, io:device()} | {stdin, standard_io}.
-type env() :: [any()].
-type lobject() :: {fixnum, integer()}
    | {boolean, boolean()} 
    | {symbol, string()} 
    | nil 
    | {pair, lobject(), lobject()} 
    | {primitive, string(), function()} 
    | {quote, lobject()} 
    | {closure, [string()], any(), env()}.

-type exp() :: {literal, lobject()}
    | {var, string()}
    | {if_, exp(), exp(), exp()}
    | {and_, exp(), exp()}
    | {or_, exp(), exp()}
    | {apply, exp(), exp()}
    | {call, exp(), [exp()]}
    | {lambda, [string()], exp()}
    | {let_, let_kind(), [{string(), exp()}], exp()}
    | {defexp, def()}.

-type let_kind() :: let_ | letstar | letrec.

-type def() :: {val, string(), exp()}
    | {def, string(), [string()], exp()}
    | {exp, exp()}.

-record(stream, {
    line_num = 1,
    chars = [] :: [char()],
    chan :: io:device()
}).

is_digit(C) -> C >= $0 andalso C =< $9.

is_white(C) -> C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r.

is_alpha(C) -> (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z).

is_symstartchar(C) -> 
    is_alpha(C) orelse lists:member(C, "*/><=?!-").

is_delimiter(C) -> 
    lists:member(C, "()[]{};\"") orelse is_white(C).

read_char(Stm = #stream{chars = [C | Rest]}) -> 
    NewStm = case C of
        $\n -> Stm#stream{chars = Rest, line_num = Stm#stream.line_num + 1};
        _ -> Stm#stream{chars = Rest}
    end,
    {C, NewStm};
read_char(Stm = #stream{chars = [], chan = {_, Io}}) -> 
    case io:get_chars(Io, "", 1) of
        eof -> {eof, Stm};
        {error, Reason} -> erlang:error({io_error, Reason});
        [C] -> 
            NewStm = case C of
                $\n -> Stm#stream{line_num = Stm#stream.line_num + 1};
                _ -> Stm
            end,
            {C, NewStm}
    end.

unread_char(Stm, eof) -> Stm;
unread_char(Stm, C) -> 
    Stm#stream{chars = [C | Stm#stream.chars]}.

consume_white(Stm) ->
    case read_char(Stm) of
        {eof, Stm1} -> Stm1;
        {C, Stm1} ->
            case is_white(C) of
                true -> consume_white(Stm1);
                false -> unread_char(Stm1, C)
            end
    end.

handle_comment(Stm) ->
    case read_char(Stm) of
        {eof, Stm1} -> Stm1;
        {$\n, Stm1} -> Stm1;
        {_, Stm1} -> handle_comment(Stm1)
    end.

read_symbol(Stm) ->
    read_symbol(Stm, []).

read_symbol(Stm, Acc) ->
    {C, Stm1} = read_char(Stm),
    case C =/= eof andalso not is_delimiter(C) of
        true -> read_symbol(Stm1, [C | Acc]);
        false -> {lists:reverse(Acc), unread_char(Stm1, C)}
    end.

read_fixnum(Stm, Acc) ->
    {C, Stm1} = read_char(Stm),
    case is_digit(C) of
        true -> read_fixnum(Stm1, [C | Acc]);
        false -> 
            NumStr = lists:reverse(Acc),
            {Num, _} = string:to_integer(NumStr),
            {{fixnum, Num}, unread_char(Stm1, C)}
    end.

read_list(Stm) ->
    Stm1 = consume_white(Stm),
    {C, Stm2} = read_char(Stm1),
    case C of
        $) -> {nil, Stm2};
        eof -> erlang:error(unexpected_eof);
        _ ->
            {Car, Stm3} = read_sexp(unread_char(Stm2, C)),
            {Cdr, Stm4} = read_list(Stm3),
            {{pair, Car, Cdr}, Stm4}
    end.

read_sexp(Stm) ->
    Stm1 = consume_white(Stm),
    {C, Stm2} = read_char(Stm1),
    case C of
        eof -> {eof, Stm2};
        $; -> read_sexp(handle_comment(Stm2));
        $# -> 
            {C2, Stm3} = read_char(Stm2),
            case C2 of
                $t -> {{boolean, true}, Stm3};
                $f -> {{boolean, false}, Stm3};
                _ -> erlang:error({syntax_error, "Invalid boolean"})
            end;
        $( -> read_list(Stm2);
        $' -> 
            {Obj, Stm3} = read_sexp(Stm2),
            {{quote, Obj}, Stm3};
        $~ -> 
            read_fixnum(Stm2, "-");
        _ when C >= $0, C =< $9 -> 
            read_fixnum(Stm2, [C]);
        _ ->
            case is_symstartchar(C) of
                true ->
                    {SymRest, Stm3} = read_symbol(Stm2),
                    {{symbol, [C | SymRest]}, Stm3};
                false ->
                    erlang:error({syntax_error, "Unexpected char " ++ [C]})
            end
    end.

cond_to_if([]) -> {literal, nil};
cond_to_if([{pair, {symbol, "else"}, {pair, Exp, nil}} | _]) ->
    build_ast(Exp);
cond_to_if([{pair, Cond, {pair, Exp, nil}} | T]) ->
    {if_, build_ast(Cond), build_ast(Exp), cond_to_if(T)};
cond_to_if(_) ->
    erlang:error({syntax_error, "Invalid cond syntax"}).

lobject_to_list(nil) -> [];
lobject_to_list({pair, Car, Cdr}) -> [Car | lobject_to_list(Cdr)];
lobject_to_list(Other) -> erlang:error({type_error, "Not a proper list", Other}).

is_lobject_list(nil) -> true;
is_lobject_list({pair, _, Cdr}) -> is_lobject_list(Cdr);
is_lobject_list(_) -> false.

-spec build_ast(lobject()) -> exp().
build_ast({fixnum, _} = Obj) -> {literal, Obj};
build_ast({boolean, _} = Obj) -> {literal, Obj};
build_ast(nil) -> {literal, nil};
build_ast({quote, _} = Obj) -> {literal, Obj};
build_ast({symbol, S}) -> {var, S};
build_ast(Sexp = {pair, _, _}) -> 
    case is_lobject_list(Sexp) of 
        true -> 
            List = lobject_to_list(Sexp),
            case List of
                [{symbol, "if"}, Cond, Iftrue, Iffalse] -> 
                    {if_, build_ast(Cond), build_ast(Iftrue), build_ast(Iffalse)};
                [{symbol, "and"}, C1, C2] -> 
                    {and_, build_ast(C1), build_ast(C2)};
                [{symbol, "or"}, C1, C2] -> 
                    {or_, build_ast(C1), build_ast(C2)};
                [{symbol, "quote"}, Sexp] -> 
                    {literal, {quote, Sexp}};
                [{symbol, "lambda"}, NamesObj, Body] ->
                    case is_lobject_list(NamesObj) of
                        true ->
                            Names = [case S of {symbol, Name} -> Name; _ -> erlang:error({type_error, "Lambda formals must be symbols"}) end || S <- lobject_to_list(NamesObj)],
                            {lambda, Names, build_ast(Body)};
                        false -> erlang:error({type_error, "Lambda formals must be a list"})
                    end;
                [{symbol, "define"}, {symbol, N}, NamesObj, Body] ->
                    Names = [case S of {symbol, Name} -> Name; _ -> erlang:error(type_error) end || S <- lobject_to_list(NamesObj)],
                    {defexp, {def, N, Names, build_ast(Body)}};
                [{symbol, "apply"}, Fnexp, Args] -> 
                    {apply, build_ast(Fnexp), build_ast(Args)};
                [{symbol, "cond"} | Conditions] -> 
                    cond_to_if(Conditions);
                [{symbol, S}, Bindings, Exp] when S =:= "let"; S =:= "let*"; S =:= "letrec" ->
                    Kind = case S of "let" -> let_; "let*" -> letstar; "letrec" -> letrec end,
                    ParsedBindings = [parse_binding(B) || B <- lobject_to_list(Bindings)],
                    assert_unique([Name || {Name, _} <- ParsedBindings]),
                    {let_, Kind, ParsedBindings, build_ast(Exp)};
                [Fnexp | Args]  -> 
                    {call, build_ast(Fnexp), [build_ast(S) || S <- Args]};
                [] -> erlang:error({parse_error, "Poorly formed expression"})
            end;
        false -> {literal, Sexp}
    end;
build_ast({primitive, _, _} = _) -> 
    erlang:error({syntax_error, "Primitive cannot appear in source"}).

parse_binding({pair, {symbol, N}, {pair, E, nil}}) ->
    {N, build_ast(E)};
parse_binding(_) ->
    erlang:error({type_error, "Let binding must be (name exp)"}).

assert_unique(Names) ->
    case length(Names) =:= length(lists:usort(Names)) of
        true -> ok;
        false -> erlang:error({type_error, "Duplicate names in let bindings"})
    end.

string_val(Val) ->
    Val.

eval(Ast, Env) ->
    {Ast, 1}.

repl(Stm, Env) ->
    repl(Stm, Env, element(1, Stm#stream.chan) =:= stdin).
repl(Stm, Env, true) ->  
    io:format("> ", []),
    do_repl(Stm, Env, true);
repl(Stm, Env, false) ->
    do_repl(Stm, Env, false).

do_repl(Stm, Env, IsStdin) ->
    {Sexp, Stm1} = read_sexp(Stm),
    Ast = build_ast(Sexp),
    {Result, NewEnv} = eval(Ast, Env),
    case IsStdin of
        true -> io:format("~p~n", [string_val(Result)]);
        false -> ok
    end,
    repl(Stm1, NewEnv).


-spec get_ic() -> chan().
get_ic() -> 
    case init:get_plain_arguments() of
        [Path | _] -> 
            case file:open(Path, [read]) of 
                {ok, IoDevice} -> 
                    {file, IoDevice};
                {error, _} -> 
                    {stdin, standard_io}
            end;
        [] -> 
            {stdin, standard_io}
    end.

main() -> 
    {State, IoDevice} = get_ic(),
    try 
        repl (#stream{chan={State, IoDevice}}, stdlib())
    catch
        error:eof -> handle_eof(State, IoDevice)
    end.

handle_eof(file, IoDevice) ->
    io:format("EOF reached, closing file~n"),
    file:close(IoDevice);
handle_eof(stdin, _) ->
    io:format("EOF on stdin, exiting~n").

stdlib() ->
    [].
