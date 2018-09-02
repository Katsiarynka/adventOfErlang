-module(template).

-export([parse/2]).

replace_matched(Key, Value, Str) ->
	KeyInStr = "{{" + Key + "}}",
	binary:replace(Str, KeyInStr, Value, [global]).


parse(Str, Data) when is_binary(Str) ->
    lists:map(fun(X) -> replace_matched(X, maps:get(X), Str) end, maps:keys(Data)).