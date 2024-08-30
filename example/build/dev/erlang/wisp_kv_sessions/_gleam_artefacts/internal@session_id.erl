-module(internal@session_id).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([generate/0, to_string/1]).
-export_type([session_id/0]).

-type session_id() :: {session_id, binary()}.

-spec generate() -> session_id().
generate() ->
    {session_id, wisp:random_string(200)}.

-spec to_string(session_id()) -> binary().
to_string(Id) ->
    case Id of
        {session_id, Id_string} ->
            Id_string
    end.
