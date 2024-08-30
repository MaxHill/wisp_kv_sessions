-module(internal@utils).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([remove_cookie/2, inject_session_cookie/4, set_session_cookie/4, get_session_id/2]).

-spec remove_cookie(
    gleam@http@request:request(wisp@internal:connection()),
    binary()
) -> gleam@http@request:request(wisp@internal:connection()).
remove_cookie(Req, Name) ->
    {Cookies_string@1, Headers@1} = case gleam@list:key_pop(
        erlang:element(3, Req),
        <<"cookie"/utf8>>
    ) of
        {ok, {Cookies_string, Headers}} ->
            New_cookie_string = begin
                _pipe = gleam@string:split(Cookies_string, <<";"/utf8>>),
                _pipe@1 = gleam@list:map(_pipe, fun gleam@string:trim/1),
                _pipe@3 = gleam@list:filter(
                    _pipe@1,
                    fun(Str) ->
                        gleam@io:debug(Str),
                        _pipe@2 = gleam@string:starts_with(Str, Name),
                        gleam@bool:negate(_pipe@2)
                    end
                ),
                gleam@string:join(_pipe@3, <<";"/utf8>>)
            end,
            {New_cookie_string, Headers};

        {error, _} ->
            {<<""/utf8>>, erlang:element(3, Req)}
    end,
    erlang:setelement(
        3,
        Req,
        [{<<"cookie"/utf8>>, Cookies_string@1} | Headers@1]
    ).

-spec inject_session_cookie(
    binary(),
    gleam@http@request:request(wisp@internal:connection()),
    internal@session_id:session_id(),
    wisp:security()
) -> gleam@http@request:request(wisp@internal:connection()).
inject_session_cookie(Cookie_name, Req, Session_id, Security) ->
    Value = internal@session_id:to_string(Session_id),
    Value@1 = case Security of
        plain_text ->
            gleam_stdlib:bit_array_base64_encode(<<Value/binary>>, false);

        signed ->
            wisp:sign_message(Req, <<Value/binary>>, sha512)
    end,
    _pipe = Req,
    _pipe@1 = remove_cookie(_pipe, Cookie_name),
    gleam@http@request:set_cookie(_pipe@1, Cookie_name, Value@1).

-spec seconds_from_now(birl:time()) -> integer().
seconds_from_now(Time) ->
    _pipe = Time,
    _pipe@1 = birl:difference(_pipe, birl:now()),
    birl@duration:blur_to(_pipe@1, second).

-spec set_session_cookie(
    binary(),
    gleam@http@response:response(wisp:body()),
    gleam@http@request:request(wisp@internal:connection()),
    wisp_kv_sessions@session:session()
) -> gleam@http@response:response(wisp:body()).
set_session_cookie(Cookie_name, Response, Req, Session) ->
    wisp:set_cookie(
        Response,
        Req,
        Cookie_name,
        internal@session_id:to_string(erlang:element(2, Session)),
        signed,
        seconds_from_now(erlang:element(3, Session))
    ).

-spec get_session_id(
    binary(),
    gleam@http@request:request(wisp@internal:connection())
) -> {ok, internal@session_id:session_id()} |
    {error, internal@session_error:session_error()}.
get_session_id(Cookie_name, Req) ->
    _pipe = wisp:get_cookie(Req, Cookie_name, signed),
    _pipe@1 = gleam@result:replace_error(_pipe, no_session_cookie_error),
    gleam@result:map(_pipe@1, fun(Field@0) -> {session_id, Field@0} end).
