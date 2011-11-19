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
    case [ Err || Err <- lists:map(
        fun({command, _, PhaseDepends, PhaseCommands}) ->
            case PhaseDepends of
                [] ->
                    ok;
                Deps when is_list(Deps) ->
                    [ execute_command(C, Root,
                                    Config, AppFile) || C <- Deps ];
                Dep when is_atom(Dep) ->
                    execute_command(Dep, Root, Config, AppFile)
            end,
            rebar_log:log(info, "Processing phases ~p~n", [PhaseCommands]),
            rebar_core:process_commands(PhaseCommands, Config);
         (C) when is_atom(C) ->
             rebar_log:log(info, "Processing phase ~p~n", [C]),
             rebar_core:process_commands([C], Config)
        end, lookup_phase_config(Command, Config)), Err /= ok ] of
        [] ->
            ok;
        Other ->
            {error, Other}
    end.

%%
%% Internal API
%%

lookup_phase_config(Command, Config) ->
    [ C || {command, Cmd, _, _}=C <- commands(Config), Cmd == Command ].

commands(Config) ->
    case load_phases(Config) of
        [] ->
            [];
        Phases when is_list(Phases) ->
            [ generate_command(Phase) || Phase <- Phases ];
        LifecycleDefinition when is_atom(LifecycleDefinition) ->
            %% this only runs in base_dir, so we can go into deps and look
            %% TODO: have this set up to work
            AppPath = case code:lib_dir(LifecycleDefinition) of
                {error, _} ->
                    filename:join(deps_dir(), atom_to_list(LifecycleDefinition));
                P when is_list(P) ->
                    P
            end,
            case rebar_utils:find_files(AppPath, "lifecycle.config") of
                [] ->
                    rebar_utils:abort("Unable to locate ~p in ~s~n",
                                        [LifecycleDefinition, AppPath]);
                [ConfigPath] ->
                    Cfg = rebar_config:new(ConfigPath),
                    rebar_config:get_local(Cfg, phases, [])
            end
    end.

load_phases(Config) ->
    case rebar_config:get_global({phases, Config}, undefined) of
        undefined ->
            case rebar_config:get_local(Config, phases, []) of
                [] ->
                    rebar_config:get(Config, phases, []);
                Defs ->
                    Defs
            end;
        Phases ->
            Phases
    end.

generate_command({PhaseName, PhaseCommands}) ->
    generate_command({PhaseName, [], PhaseCommands});
generate_command({PhaseName, PhaseDepends, PhaseCommands}) ->
    {command, PhaseName, PhaseDepends, PhaseCommands}.

deps_dir() ->
    rebar_config:get_global(deps_dir, rebar_config:get_local(deps_dir, "deps")).

basename() ->
    filename:basename(basedir()).

basedir() ->
    rebar_config:get_global(base_dir, undefined).

is_basedir() ->
    rebar_utils:get_cwd() == basedir().
