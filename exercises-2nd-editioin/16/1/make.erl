-module(make).
-export([need_recompile/1]).
-include_lib("kernel/include/file.hrl").

need_recompile(Mod) when is_atom(Mod) ->
    need_recompile(atom_to_list(Mod));
need_recompile(Mod) ->
    {ok, Erl} = file:read_file_info(lists:append(Mod, ".erl")),
    {ok, Beam} = file:read_file_info(lists:append(Mod, ".beam")),
    need_recompile1(Erl, Beam).

need_recompile1(#file_info{mtime=Te}, #file_info{mtime=To}) ->
    Te > To.
    
