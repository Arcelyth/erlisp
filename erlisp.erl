-module(erlisp).
-export([main/0]).
-type chan() :: {file, io:device()} | {stdin, standard_io}.
-type env() :: [any()].
-type lobject() :: {fixnum, integer()}
    | {boolean, boolean()} 
    | {string, string()}
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
    | {let_exp, let_kind(), [{string(), exp()}], exp()}
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
    is_alpha(C) orelse lists:member(C, "*/><=?!-+").

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

read_string(Stm, Acc) -> 
    {C, Stm1} = read_char(Stm),
    case C of 
        $" -> 
            Str = lists:reverse(Acc),
            {{string, Str}, Stm1};
        _ -> read_string(Stm1, [C | Acc])
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
        $" -> 
            read_string(Stm2, []);
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
build_ast({string, _} = Obj) -> {literal, Obj};
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
                [{symbol, "quote"}, Target] -> 
                    {literal, {quote, Target}};
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
                [{symbol, "val"}, {symbol, N}, E] -> {defexp, {val, N, build_ast(E)}};
                [{symbol, S}, Bindings, Exp] when S =:= "let"; S =:= "let*"; S =:= "letrec" ->
                    Kind = case S of "let" -> let_; "let*" -> letstar; "letrec" -> letrec end,
                    ParsedBindings = [parse_binding(B) || B <- lobject_to_list(Bindings)],
                    assert_unique([Name || {Name, _} <- ParsedBindings]),
                    {let_exp, Kind, ParsedBindings, build_ast(Exp)};
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

string_list({pair, Car, nil}) -> string_val(Car);
string_list({pair, Car, Cdr}) -> string_val(Car) ++ " " ++ string_list(Cdr);
string_list(_) -> 
    erlang:error({type_error, "expect pair"}).

string_pair({pair, Car, Cdr}) -> string_val(Car) ++ " . " ++ string_val(Cdr);
string_pair(_) -> 
    erlang:error({type_error, "expect pair"}).

string_val({fixnum, V}) -> integer_to_list(V);
string_val({string, S}) -> S;
string_val({boolean, true}) -> "#t";
string_val({boolean, false}) -> "#f";
string_val({symbol, S}) -> S;
string_val(nil) -> "nil";
string_val(Sexp = {pair, _Car, _Cdr}) -> 
    V = case is_lobject_list(Sexp) of
            true -> string_list(Sexp);
            false -> string_pair(Sexp)
        end,
    "(" ++ V ++ ")";
string_val({primitive, Name, _}) -> "#<primitive:" ++ Name ++ ">";
string_val({quote, Q}) -> "'" ++ string_val(Q);
string_val({closure, _, _, _}) -> "#<closure>".

bind(Name, Value, Env) ->
    [{Name, Value} | Env].
lookup(N, Env) ->
    case lists:keyfind(N, 1, Env) of
        {N, V} -> V;
        false -> erlang:error({unbound_variable, N})
    end.

env_to_val([]) -> 
    nil;
env_to_val([{Name, Value} | Rest]) ->
    {pair, bind_to_val(Name, Value), env_to_val(Rest)}.

bind_to_val(Name, undefined) ->
    {pair, {symbol, Name}, {symbol, "unspecified"}};
bind_to_val(Name, Value) ->
    {pair, {symbol, Name}, Value}.

fix_env(Env, _FullEnv, 0) -> Env;
fix_env([{N, {closure, Ns, B, _OldEnv}} | T], FullEnv, Count) ->
    [{N, {closure, Ns, B, FullEnv}} | fix_env(T, FullEnv, Count - 1)];
fix_env([H | T], FullEnv, Count) ->
    [H | fix_env(T, FullEnv, Count)].

eval_apply({primitive, _, F}, Es) -> F(Es);
eval_apply({closure, Names, Body, ClEnv}, Es) -> 
    NewEnv = lists:zip(Names, Es) ++ ClEnv,
    eval_exp(Body, NewEnv);
eval_apply(_, _) -> erlang:error({type_error, "(apply prim '(args)) or (prim args)"}).

do_eval({literal, {quote, Q}}, _Env) -> Q;
do_eval({literal, L}, _Env) -> L;
do_eval({var, Name}, Env) -> lookup(Name, Env);
do_eval({if_, Cond, T, F}, Env) -> 
    case do_eval(Cond, Env) of 
        {boolean, true} -> do_eval(T, Env);
        {boolean, false} -> do_eval(F, Env);
        _ -> erlang:error({type_error, "(if bool e1 e2)"})
    end;
do_eval({and_, C1, C2}, Env) ->
    case {do_eval(C1, Env), do_eval(C2, Env)} of 
        {{boolean, V1}, {boolean, V2}} -> {boolean, V1 and V2};
        _ -> erlang:error({type_error, "(and bool bool)"})
    end;
do_eval({or_, C1, C2}, Env) -> 
    case {do_eval(C1, Env), do_eval(C2, Env)} of 
        {{boolean, V1}, {boolean, V2}} -> {boolean, V1 or V2};
        _ -> erlang:error({type_error, "(or bool bool)"})
    end;
do_eval({apply, Fn, ArgsExp}, Env) -> 
    eval_apply(do_eval(Fn, Env), lobject_to_list(do_eval(ArgsExp, Env)));
do_eval({call, {var, "env"}, []}, Env) -> env_to_val(Env);
do_eval({call, Fn, ArgsExps}, Env) -> 
    Args = [do_eval(A, Env) || A <- ArgsExps],
    eval_apply(do_eval(Fn, Env), Args);
do_eval({lambda, Names, Val}, Env) -> {closure, Names, Val, Env};
do_eval({let_exp, let_, Bs, Body}, Env) ->
    NewBindings = [{N, do_eval(E, Env)} || {N, E} <- Bs],
    eval_exp(Body, NewBindings ++ Env);
do_eval({let_exp, letstar, Bs, Body}, Env) ->
    FinalEnv = lists:foldl(fun({N, E}, AccEnv) ->
        [{N, do_eval(E, AccEnv)} | AccEnv]
    end, Env, Bs),
    eval_exp(Body, FinalEnv);
do_eval({let_exp, letrec, Bs, Body}, Env) ->
    Names = [N || {N, _} <- Bs],
    RecEnv = lists:foldl(fun({N, E}, AccEnv) ->
        case E of
            {lambda, Ns, BodyExp} -> 
                [{N, {closure, Ns, BodyExp, AccEnv}} | AccEnv];
            _ -> 
                [{N, do_eval(E, Env)} | AccEnv]
        end
    end, Env, Bs),
    FixedEnv = fix_env(RecEnv, RecEnv, length(Names)),
    eval_exp(Body, FixedEnv);
do_eval(_, _Env) -> 
    erlang:error({type_error, "unknown type"}).

eval_exp(Exp, Env) ->
    try
        do_eval(Exp, Env)
    catch
        error:Reason ->
            erlang:error(Reason)
    end.

eval_def({val, Name, ValExp}, Env) -> 
    V = eval_exp(ValExp, Env),
    {V, bind(Name, V, Env)};  
eval_def({def, Name, Names, E}, Env) -> 
    case eval_exp({lambda, Names, E}, Env) of
        {closure, Formals, Body, ClEnv} ->
            RecClo = {closure, Formals, Body, [{Name, {closure, Formals, Body, ClEnv}} | ClEnv]},
            {RecClo, bind(Name, RecClo, Env)};
        _ -> 
            erlang:error({type_error, "Expecting closure in define"})
    end;
eval_def({exp, E}, Env) -> 
    {eval_exp(E, Env), Env}.

eval({defexp, Def}, Env) ->
    eval_def(Def, Env);
eval(Other, Env) ->
    {eval_exp(Other, Env), Env}.
    

repl(Stm, Env) ->
    repl(Stm, Env, element(1, Stm#stream.chan) =:= stdin).
repl(Stm, Env, true) ->  
    io:format("> ", []),
    do_repl(Stm, Env, true);
repl(Stm, Env, false) ->
    do_repl(Stm, Env, false).

do_repl(Stm, Env, IsStdin) ->
    case read_sexp(Stm) of
        {eof, _} -> 
            erlang:error(eof); 
        {Sexp, Stm1} ->
            try
                Ast = build_ast(Sexp),
                {Result, NewEnv} = eval(Ast, Env),
                if IsStdin -> io:format("~s~n", [string_val(Result)]); true -> ok end,
                repl(Stm1, NewEnv)
            catch
                error:eof -> erlang:error(eof);
                Class:Reason -> 
                    io:format("~s Error: ~p~n", [Class, Reason]),
                    repl(Stm1, Env) 
            end
    end.

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
        repl (#stream{chan={State, IoDevice}}, load_prelude())
    catch
        error:eof -> handle_eof(State, IoDevice)
    after
        init:stop()
    end.

handle_eof(file, IoDevice) ->
    io:format("EOF reached, closing file~n"),
    file:close(IoDevice);
handle_eof(stdin, _) ->
    io:format("EOF on stdin, exiting~n").

load_prelude() ->
    Dir = "./prelude/*.elp",
    Files = filelib:wildcard(Dir),
    lists:foldl(fun(Path, AccEnv) ->
        case file:open(Path, [read]) of
            {ok, IoDevice} ->
                Stm = #stream{chan = {file, IoDevice}},
                try
                    NewEnv = slurp(Stm, AccEnv),
                    file:close(IoDevice),
                    NewEnv
                catch
                    _ ->
                        file:close(IoDevice),
                        AccEnv 
                end;
            {error, _} ->
                AccEnv
        end
    end, basis(), Files).

slurp(Stm, Env) ->
    try
        case read_sexp(Stm) of 
            {eof, _} -> erlang:error(eof);
            {Sexp, Stm1} -> 
                Ast = build_ast(Sexp),
                {_Result, NewEnv} = eval_defonly(Ast, Env),
                slurp(Stm1, NewEnv)
        end
    catch
        error:eof -> Env;
        error:{type_error, Reason} ->
            io:format("Type error in prelude: ~p~n", [Reason]),
            Env
    end.

eval_defonly({defexp, Def}, Env) ->
    eval_def(Def, Env);
eval_defonly(Other, _Env) ->
    erlang:error({type_error, "Can only have definitions in prelude", Other}).

basis() ->
    NumPrim = fun(Name, Op) ->
        {Name, {primitive, Name, fun
            ([{fixnum, A}, {fixnum, B}]) -> {fixnum, Op(A, B)};
            (_) -> erlang:error({type_error, "(" ++ Name ++ " int int)"})
        end}}
    end,

    CmpPrim = fun(Name, Op) ->
        {Name, {primitive, Name, fun
            ([{fixnum, A}, {fixnum, B}]) -> {boolean, Op(A, B)};
            (_) -> erlang:error({type_error, "(" ++ Name ++ " int int)"})
        end}}
    end,

    PrimList = {"list", {primitive, "list", fun(Args) -> 
        lists:foldr(fun(X, Acc) -> {pair, X, Acc} end, nil, Args) 
    end}},

    PrimPair = {"pair", {primitive, "pair", fun
        ([A, B]) -> {pair, A, B};
        (_) -> erlang:error({type_error, "(pair a b)"})
    end}},

    PrimCar = {"car", {primitive, "car", fun
        ([{pair, Car, _}]) -> Car;
        (_) -> erlang:error({type_error, "(car non-nil-pair)"})
    end}},

    PrimCdr = {"cdr", {primitive, "cdr", fun
        ([{pair, _, Cdr}]) -> Cdr;
        (_) -> erlang:error({type_error, "(cdr non-nil-pair)"})
    end}},

    PrimAtomp = {"atom?", {primitive, "atom?", fun
        ([{pair, _, _}]) -> {boolean, false};
        ([_]) -> {boolean, true};
        (_) -> erlang:error({type_error, "(atom? something)"})
    end}},

    PrimSymp = {"sym?", {primitive, "sym?", fun
        ([{symbol, _}]) -> {boolean, true};
        ([_]) -> {boolean, false};
        (_) -> erlang:error({type_error, "(sym? single-arg)"})
    end}},

    PrimEq = {"eq", {primitive, "eq", fun
        ([A, B]) -> {boolean, A =:= B};
        (_) -> erlang:error({type_error, "(eq a b)"})
    end}},

    PrimGetchar = {"getchar", {primitive, "getchar", fun
        ([]) -> 
            case io:get_chars(standard_io, "", 1) of
                eof -> {fixnum, -1};
                [C] -> {fixnum, C}
            end;
        (_) -> erlang:error({type_error, "(getchar)"})
    end}},

    PrimPrint = {"print", {primitive, "print", fun
        ([V]) -> 
            io:format("~s", [string_val(V)]),
            {symbol, "ok"};
        (_) -> erlang:error({type_error, "(print val)"})
    end}},

    PrimItoc = {"itoc", {primitive, "itoc", fun
        ([{fixnum, I}]) -> {symbol, [I]};
        (_) -> erlang:error({type_error, "(itoc int)"})
    end}},

    PrimCat = {"cat", {primitive, "cat", fun
        ([{symbol, A}, {symbol, B}]) -> {symbol, A ++ B};
        (_) -> erlang:error({type_error, "(cat sym sym)"})
    end}},

    [
        NumPrim("+", fun(A, B) -> A + B end),
        NumPrim("-", fun(A, B) -> A - B end),
        NumPrim("*", fun(A, B) -> A * B end),
        NumPrim("/", fun(A, B) -> A div B end),
        CmpPrim("<", fun(A, B) -> A < B end),
        CmpPrim(">", fun(A, B) -> A > B end),
        CmpPrim("=", fun(A, B) -> A =:= B end),
        PrimList, PrimPair, PrimCar, PrimCdr,
        PrimAtomp, PrimSymp, PrimEq,
        PrimGetchar, PrimPrint, PrimItoc, PrimCat
    ].

