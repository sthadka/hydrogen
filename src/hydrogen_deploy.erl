%%%-------------------------------------------------------------------
%%% @doc hydrogen deploy
%%%
%%% Helper for deploying code
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_deploy).

%% TODO: Handle different folder structures apps, lib etc.

-export([reload_release/1,
         reload_app/1,
         load_app/1,
         modules_for_app/1,
         reload_module/1, reload_module/2,
         add_paths/1,
         modified_deps/0,
         is_latest_version/1,
         module_modified/1,
         find_module_file/1,
         code_change/3,
         old_code/1,
         stuck_old_code/1]).

reload_release(App) ->
    ok = add_paths("lib"),
    reload_code(App).

reload_app(App) ->
    ok = add_paths("deps"),
    reload_code(App).

load_app(App) ->
    [code:load_file(M) || M <- modules_for_app(App)].

modules_for_app(App) ->
    case code:where_is_file(atom_to_list(App) ++ ".app") of
        non_existing ->
            {error, no_app_file};
        Path ->
            case file:consult(Path) of
                {ok, [{application, App, Props}]} ->
                    proplists:get_value(modules, Props, []);
                {error, Error} ->
                    {error, Error}
            end
    end.

reload_module(M) ->
    code:soft_purge(M) andalso code:load_file(M).

reload_module(M, force) ->
    code:purge(M),
    code:load_file(M).

add_paths(BaseDir) ->
    case file:list_dir(BaseDir) of
        {ok, Filenames} ->
            lists:foreach(fun (Dep) ->
                              true == code:add_path(
                                          filename:join([BaseDir, Dep, "ebin"]))
                          end, Filenames),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% list loaded modules from deps that have changed on disk,
%% will not notice new beams that have not yet been loaded.
modified_deps() ->
    DepModules = deps_modules(),
    lists:filter(fun (Module) -> not is_latest_version(Module) end, DepModules).

%% check loaded vsn versus vsn on disk, do this since compile times changes
%% frequently for deps even if source has not (because of delete-deps).
is_latest_version(Module) ->
    case code:get_object_code(Module) of
        {Module, _ObjectCode, Beam} ->
            {ok, {Module, VsnDisk}} = beam_lib:version(Beam),
            %% Can't use object code since it is read from disk
            VsnLoaded = proplists:get_value(vsn, Module:module_info(attributes)),
            VsnLoaded =:= VsnDisk;
        error ->
            %% Module not loaded
            false
    end.

%% snipped from Wings3d
module_modified(Module) ->
    case code:is_loaded(Module) of
        {file, preloaded} ->
            false;
        {file, Path} ->
            CompileOpts = proplists:get_value(compile, Module:module_info()),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            module_modified(Path, CompileTime, Src);
        _ ->
            true %% consider new modules to be modified so they get loaded
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
        false ->
            false;
        ModPath ->
            {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
            CompileOpts =  binary_to_term(CB),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            not ((CompileTime == PrevCompileTime) and (Src == PrevSrc))
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
        {ok, _} ->
            Path;
        _ ->
            %% may be the path was changed?
            case code:where_is_file(filename:basename(Path)) of
                non_existing ->
                    false;
                NewPath ->
                    NewPath
            end
    end.

%% hot code reload for modules with modified state
code_change(Module, Vsn, Extra) ->
    ok = sys:suspend(Module),
    % Do not purge if process is still using old code
    true = code:soft_purge(Module),
    {module, Module} = code:load_file(Module),
    ok = sys:change_code(Module, Module, Vsn, Extra),
    ok = sys:resume(Module).

old_code(Apps) ->
    lists:filter(fun erlang:check_old_code/1,
                 lists:flatten(
                    [deps_modules() | [modules_for_app(App) || App <- Apps]])).

%% returns a list of modules for which at least one process is using old code
stuck_old_code(Apps) ->
    %% NOTE: soft purging a lot of modules can take a while!!
    lists:filter(fun (M) -> not code:soft_purge(M) end, old_code(Apps)).

%%-------------------------------------------------------------------
%% Internal helpers
%%-------------------------------------------------------------------

reload_code(App) ->
    ModifiedModules = lists:filter(fun module_modified/1, modules_for_app(App)),
    [] = [M || M <- ModifiedModules, code:soft_purge(M) =:= false],
    [code:load_file(M) || M <- ModifiedModules].

deps_modules() ->
    {DepModules, _} = lists:unzip(lists:filter(
                        fun ({_, Beam}) ->
                                is_list(Beam) andalso
                                    lists:prefix(filename:absname("deps"), Beam)
                        end,
                        code:all_loaded())),
    DepModules.
