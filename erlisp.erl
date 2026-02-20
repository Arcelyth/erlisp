-module(erlisp).
-export([main/0]).

-record(stream, {line_num=1,
                chars=[],
                chan}).

repl(_Stm, _Env) -> 
    ok.

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
        repl (#stream{chan=IoDevice}, stdlib())
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




