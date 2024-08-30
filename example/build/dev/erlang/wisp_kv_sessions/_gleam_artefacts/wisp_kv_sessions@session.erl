-module(wisp_kv_sessions@session).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([session_get/2, session_expires_at/1, builder_from/1, builder/0, with_id/2, with_id_string/2, with_expiry/2, with_expires_at/2, with_data/2, set_key_value/3, expiry_to_date/1, build/1]).
-export_type([session/0, expiry/0, session_builder/0]).

-type session() :: {session,
        internal@session_id:session_id(),
        birl:time(),
        gleam@dict:dict(binary(), gleam@json:json())}.

-type expiry() :: {expire_at, birl:time()} | {expire_in, integer()}.

-type session_builder() :: {session_builder,
        internal@session_id:session_id(),
        gleam@option:option(expiry()),
        gleam@dict:dict(binary(), gleam@json:json())}.

-spec session_get(session(), binary()) -> {ok, gleam@json:json()} | {error, nil}.
session_get(Session, Key) ->
    gleam@dict:get(erlang:element(4, Session), Key).

-spec session_expires_at(session()) -> birl:time().
session_expires_at(Session) ->
    erlang:element(3, Session).

-spec builder_from(session()) -> session_builder().
builder_from(Session) ->
    {session_builder,
        erlang:element(2, Session),
        {some, {expire_at, erlang:element(3, Session)}},
        erlang:element(4, Session)}.

-spec builder() -> session_builder().
builder() ->
    {session_builder, internal@session_id:generate(), none, gleam@dict:new()}.

-spec with_id(session_builder(), internal@session_id:session_id()) -> session_builder().
with_id(Session, Id) ->
    erlang:setelement(2, Session, Id).

-spec with_id_string(session_builder(), binary()) -> session_builder().
with_id_string(Session, Id) ->
    erlang:setelement(2, Session, {session_id, Id}).

-spec with_expiry(session_builder(), expiry()) -> session_builder().
with_expiry(Session, Expiry) ->
    erlang:setelement(3, Session, {some, Expiry}).

-spec with_expires_at(session_builder(), birl:time()) -> session_builder().
with_expires_at(Session, Expires_at) ->
    erlang:setelement(3, Session, {some, {expire_at, Expires_at}}).

-spec with_data(session_builder(), gleam@dict:dict(binary(), gleam@json:json())) -> session_builder().
with_data(Session, Data) ->
    erlang:setelement(4, Session, Data).

-spec set_key_value(session_builder(), binary(), gleam@json:json()) -> session_builder().
set_key_value(Session, Key, Data) ->
    erlang:setelement(
        4,
        Session,
        gleam@dict:insert(erlang:element(4, Session), Key, Data)
    ).

-spec expiry_to_date(expiry()) -> birl:time().
expiry_to_date(Expiry) ->
    case Expiry of
        {expire_at, Time} ->
            Time;

        {expire_in, Seconds} ->
            _pipe = birl:now(),
            birl:add(_pipe, birl@duration:seconds(Seconds))
    end.

-spec build(session_builder()) -> session().
build(Session) ->
    Expiry = gleam@option:unwrap(
        erlang:element(3, Session),
        {expire_in, 60 * 60}
    ),
    {session,
        erlang:element(2, Session),
        expiry_to_date(Expiry),
        erlang:element(4, Session)}.
