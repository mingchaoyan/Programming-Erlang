-module(loglevel).

-export([set/1,
         get/0,
         set_custom/2,
         clear_custom/0,
         clear_custom/1,
         set_loglevel/1,
         print/2
        ]).

-include("logger.hrl").

-define(LOGMODULE, "error_logger").

%% Error levels:
-record(loglevel, {ordinal,
                   name,
                   description,
                   function = no_log,
                   event_type = no_log,
                   msg_prefix = no_log}).

-define(LOG_LEVELS,
        [#loglevel{ordinal = 0, name = no_log, description = "No log"},
         #loglevel{ordinal = 1, name = critical, description = "Critical",
                   function = critical_msg, event_type = error, msg_prefix = "C"},
         #loglevel{ordinal = 2, name = error, description = "Error",
                   function = error_msg, event_type = error, msg_prefix = "E"},
         #loglevel{ordinal = 3, name = warning, description = "Warning",
                   function = warning_msg, event_type = warning_msg, msg_prefix = "W"},
         #loglevel{ordinal = 4, name = info, description = "Info",
                   function = info_msg, event_type = info_msg, msg_prefix = "I"},
         #loglevel{ordinal = 5, name = debug, description = "Debug",
                   function = debug_msg, event_type = info_msg, msg_prefix = "D"}]).

%% @type level() = integer() | atom().

%% @spec () -> {DefaultLevelOrdinal::integer(), [{Module::atom(), LevelOrdinal::integer()}]}
%% @doc Get the default and all custom levels
get() ->
    {DefaultLevel, _CustomLevels} = logger:get(),
    case lists:keysearch(DefaultLevel, #loglevel.ordinal, ?LOG_LEVELS) of
        {value, Result = #loglevel{}} ->
            {Result#loglevel.ordinal, Result#loglevel.name, Result#loglevel.description};
        _ ->
            erlang:error({no_such_loglevel, DefaultLevel})
    end.

%% @spec (DefaultLevel::level() | {DefaultLevel::level(), [{Module::atom(), Level::level()}]}) ->
%%       {module, logger}
%% @doc Set the default and all custom levels
set(DefaultLevel) when is_atom(DefaultLevel) orelse is_integer(DefaultLevel) ->
    set({DefaultLevel, []});
set({DefaultLevel, CustomLevels}) when is_list(CustomLevels) ->
    DefaultInt = level_to_integer(DefaultLevel),
    CustomInts = [level_to_integer(C) || C <- CustomLevels],
    Loglevel = {DefaultInt, CustomInts},
    io:format("Loglevel:~p", [Loglevel]),
    try
        {Mod,Code} = dynamic_compile:from_string(ejabberd_logger_src(Loglevel)),
        code:load_binary(Mod, ?LOGMODULE ++ ".erl", Code)
    catch
        Type:Error -> ?CRITICAL("Error compiling logger (~p): ~p~n", [Type, Error])
    end;
set(_) ->
    exit("Invalid loglevel format").

%% @spec (Module::atom(), CustomLevel::level()) -> ok
%% @doc Set a custom level
set_custom(Module, Level) ->
    {DefaultLevel, CustomLevels} = logger:get(),
    case lists:keysearch(Module, 1, CustomLevels) of
        {value, {Module, Level}} ->
            ok;
        {value, _} ->
            set({DefaultLevel, lists:keyreplace(Module, 1, CustomLevels, {Module, Level})});
        _ ->
            set({DefaultLevel, [{Module, Level} | CustomLevels]})
    end.

%% @spec () -> ok
%% @doc Clear all custom levels
clear_custom() ->
    {DefaultLevel, _CustomLevels} = logger:get(),
    set({DefaultLevel, []}).

%% @spec (Module::atom()) -> ok
%% @doc Clear a custom level
clear_custom(Module) ->
    {DefaultLevel, CustomLevels} = logger:get(),
    case lists:keysearch(Module, 1, CustomLevels) of
        {value, _} ->
            set({DefaultLevel, lists:keydelete(Module, 1, CustomLevels)});
        _ ->
            ok
    end.

level_to_integer(Level) when is_integer(Level) ->
    Level;
level_to_integer({Module, Level}) ->
    {Module, level_to_integer(Level)};
level_to_integer(Level) ->
    case lists:keysearch(Level, #loglevel.name, ?LOG_LEVELS) of
        {value, #loglevel{ordinal = Int}} -> Int;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the ejabberd logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
ejabberd_logger_src(Loglevel) ->
    lists:flatten([header_src(),
                   get_src(Loglevel),
                   [log_src(Loglevel, LevelSpec) || LevelSpec <- ?LOG_LEVELS],
                   notify_src()]).

header_src() ->
    "-module(logger).
    -export([debug_msg/5,
             info_msg/5,
             warning_msg/5,
             error_msg/5,
             critical_msg/5,
             get/0]).
    ".

get_src(Loglevel) ->
    io_lib:format("get() -> ~w.
                  ", [Loglevel]).


log_src(_Loglevel, #loglevel{function = no_log}) ->
    [];
log_src({DefaultLevel, [{Tag, Level} | Tail]}, Spec = #loglevel{ordinal = MinLevel})
        when Level < MinLevel andalso MinLevel =< DefaultLevel ->
    [atom_to_list(Spec#loglevel.function), "(", atom_to_list(Tag), ",_, _, _, _) -> ok;
     ", log_src({DefaultLevel, Tail}, Spec)];

log_src({DefaultLevel, [{Tag, Level} | Tail]}, Spec = #loglevel{ordinal = MinLevel})
        when DefaultLevel < MinLevel andalso MinLevel =< Level ->
    [atom_to_list(Spec#loglevel.function), "(", atom_to_list(Tag), " = Tag, Module, Line, Format, Args) ->",
     log_notify_src(Spec), ";
     ", log_src({DefaultLevel, Tail}, Spec)];

log_src({DefaultLevel, [_Head | Tail]}, Spec = #loglevel{}) ->
    log_src({DefaultLevel, Tail}, Spec);
log_src({DefaultLevel, []}, Spec = #loglevel{ordinal = MinLevel})
        when DefaultLevel < MinLevel ->
    [atom_to_list(Spec#loglevel.function), "(_ , _ , _, _, _) -> ok.
    "];
log_src({_DefaultLevel, []}, Spec = #loglevel{}) ->
    [atom_to_list(Spec#loglevel.function), "(Tag , Module, Line, Format, Args) ->",
     log_notify_src(Spec), ".
    "].

log_notify_src(Spec = #loglevel{}) ->
    ["notify(", atom_to_list(Spec#loglevel.event_type), ",
        \"", Spec#loglevel.msg_prefix, "(~p:[~p]:[~p.erl]:[~p]) : ~n\"++Format++\"~n\",
    [self(),Tag, Module, Line | Args])"].

notify_src() ->
            "notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".

set_loglevel(Level)->
    set(Level).

print(Tag, Args) ->
    Fun = fun(_) ->
                  "~p/"
          end,
    PP = lists:map(Fun, Args),
    Format = lists:concat([Tag, "-----------------------------------~n", lists:concat(PP)]),
    logger:debug_msg(Tag, ?MODULE, ?LINE, Format, Args).
