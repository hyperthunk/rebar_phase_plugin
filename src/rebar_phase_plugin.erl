%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(rebar_phase_plugin).
-export([preprocess/2, execute_command/4]).

preprocess(Config, _) ->
    case is_basedir() of
        true ->
            case commands(Config) of
                [] ->
                    ok;
                Cmds ->
                    rebar_cmd_builder:generate_handler(basename(),
                                                        Cmds, ?MODULE)
            end;
        false ->
            ok
    end,
    {ok, []}.

execute_command(Command, Root, Config, AppFile) ->
    lists:map(fun({command, _, PhaseDepends, PhaseCommands}) ->
                    case PhaseDepends of
                        [] ->
                            ok;
                        Deps ->
                            [ execute_command(C, Root,
                                            Config, AppFile) || C <- Deps ]
                    end,
                    rebar_core:process_commands(PhaseCommands, Config);
                 (C) when is_atom(C) ->
                    rebar_core:process_commands([C], Config)
              end, lookup_phase_config(Command, Config)).

%%
%% Internal API
%%

lookup_phase_config(Command, Config) ->
    [ C || {command, Cmd, _, _}=C <- commands(Config), Cmd == Command ].

commands(Config) ->
    case rebar_config:get_local(Config, phases, []) of
        [] ->
            [];
        Phases ->
            [ generate_command(Phase) || Phase <- Phases ]
    end.

generate_command({PhaseName, PhaseCommands}) ->
    generate_command({PhaseName, [], PhaseCommands});
generate_command({PhaseName, PhaseDepends, PhaseCommands}) ->
    {command, PhaseName, PhaseDepends, PhaseCommands}.

basename() ->
    filename:basename(basedir()).

basedir() ->
    rebar_config:get_global(base_dir, undefined).

is_basedir() ->
    rebar_utils:get_cwd() == basedir().
