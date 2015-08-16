-module(a).

-export([start/1]).
-export_type([rich_text/0]).

-opaque rich_text() :: [{integer(), integer()}].
%%-type rich_text() :: [{integer(), integer()}].

-spec start(A) -> A when
      A :: rich_text().
start(A) ->
    A.
