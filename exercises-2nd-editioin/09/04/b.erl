-module(b).

-export([start/1]).

-spec start(RichTextList) -> L when
      RichTextList :: list(a:rich_text()),
      L :: list().
start(RichTextList) ->
    RichTextList1 = a:start(RichTextList),
    [I || {I, _} <- RichTextList1].
