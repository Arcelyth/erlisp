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
read_char(Stm = #stream{chars = [], chan = Io}) -> 
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

build_ast(Sexp) ->
    ok.

string_val(Val) ->
    ok.

eval(Ast, Env) ->
    ok.

repl(Stm, Env) ->
    repl(Stm, Env, element(1, Stm#stream.chan) =:= stdin).
repl(Stm, Env, true) ->  
    io:format("> ", []),
    do_repl(Stm, Env, true);
repl(Stm, Env, false) ->
    do_repl(Stm, Env, false).

do_repl(Stm, Env, IsStdin) ->
    Ast = build_ast(read_sexp(Stm)),
    {Result, NewEnv} = eval(Ast, Env),
    case IsStdin of
        true -> io:format("~s~n", [string_val(Result)]);
        false -> ok
    end,
    repl(Stm, NewEnv).


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
