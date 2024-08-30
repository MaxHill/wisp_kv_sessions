-module(wisp_kv_sessions).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get_session/2, delete_session/2, get/4, set/5, delete/3, replace_session/4, middleware/3]).
-export_type([session_config/0, session_store/0]).

-type session_config() :: {session_config,
        wisp_kv_sessions@session:expiry(),
        binary(),
        session_store()}.

-type session_store() :: {session_store,
        integer(),
        fun((internal@session_id:session_id()) -> {ok,
                gleam@option:option(wisp_kv_sessions@session:session())} |
            {error, internal@session_error:session_error()}),
        fun((wisp_kv_sessions@session:session()) -> {ok,
                wisp_kv_sessions@session:session()} |
            {error, internal@session_error:session_error()}),
        fun((internal@session_id:session_id()) -> {ok, nil} |
            {error, internal@session_error:session_error()})}.

-spec get_session(
    session_config(),
    gleam@http@request:request(wisp@internal:connection())
) -> {ok, wisp_kv_sessions@session:session()} |
    {error, internal@session_error:session_error()}.
get_session(Config, Req) ->
    gleam@result:'try'(
        internal@utils:get_session_id(erlang:element(3, Config), Req),
        fun(Session_id) ->
            gleam@result:'try'(
                (erlang:element(3, erlang:element(4, Config)))(Session_id),
                fun(Maybe_session) -> case Maybe_session of
                        {some, Session} ->
                            {ok, Session};

                        none ->
                            _pipe = wisp_kv_sessions@session:builder(),
                            _pipe@1 = wisp_kv_sessions@session:with_id(
                                _pipe,
                                Session_id
                            ),
                            _pipe@2 = wisp_kv_sessions@session:with_expiry(
                                _pipe@1,
                                erlang:element(2, Config)
                            ),
                            _pipe@3 = wisp_kv_sessions@session:build(_pipe@2),
                            (erlang:element(4, erlang:element(4, Config)))(
                                _pipe@3
                            )
                    end end
            )
        end
    ).

-spec delete_session(
    session_config(),
    gleam@http@request:request(wisp@internal:connection())
) -> {ok, nil} | {error, internal@session_error:session_error()}.
delete_session(Config, Req) ->
    gleam@result:'try'(
        internal@utils:get_session_id(erlang:element(3, Config), Req),
        fun(Session_id) ->
            (erlang:element(5, erlang:element(4, Config)))(Session_id)
        end
    ).

-spec get(
    session_config(),
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, AZU} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, gleam@option:option(AZU)} |
    {error, internal@session_error:session_error()}.
get(Config, Req, Key, Decoder) ->
    gleam@result:'try'(
        get_session(Config, Req),
        fun(Session) ->
            case begin
                _pipe = wisp_kv_sessions@session:session_get(Session, Key),
                gleam@option:from_result(_pipe)
            end of
                none ->
                    {ok, none};

                {some, Data} ->
                    _pipe@1 = gleam@json:decode(
                        gleam@json:to_string(Data),
                        Decoder
                    ),
                    _pipe@2 = gleam@result:replace_error(_pipe@1, decode_error),
                    gleam@result:map(_pipe@2, fun(D) -> {some, D} end)
            end
        end
    ).

-spec set(
    session_config(),
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    AZX,
    fun((AZX) -> gleam@json:json())
) -> {ok, AZX} | {error, internal@session_error:session_error()}.
set(Config, Req, Key, Data, Encoder) ->
    gleam@result:'try'(
        get_session(Config, Req),
        fun(Session) ->
            Json_data = Encoder(Data),
            New_session = begin
                _pipe = wisp_kv_sessions@session:builder_from(Session),
                _pipe@1 = wisp_kv_sessions@session:set_key_value(
                    _pipe,
                    Key,
                    Json_data
                ),
                wisp_kv_sessions@session:build(_pipe@1)
            end,
            gleam@result:map(
                (erlang:element(4, erlang:element(4, Config)))(New_session),
                fun(_) -> Data end
            )
        end
    ).

-spec delete(
    session_config(),
    gleam@http@request:request(wisp@internal:connection()),
    binary()
) -> {ok, wisp_kv_sessions@session:session()} |
    {error, internal@session_error:session_error()}.
delete(Config, Req, Key) ->
    gleam@result:'try'(
        get_session(Config, Req),
        fun(Session) ->
            (erlang:element(4, erlang:element(4, Config)))(
                erlang:setelement(
                    4,
                    Session,
                    gleam@dict:delete(erlang:element(4, Session), Key)
                )
            )
        end
    ).

-spec replace_session(
    session_config(),
    gleam@http@response:response(wisp:body()),
    gleam@http@request:request(wisp@internal:connection()),
    wisp_kv_sessions@session:session()
) -> {ok, gleam@http@response:response(wisp:body())} |
    {error, internal@session_error:session_error()}.
replace_session(Config, Res, Req, New_session) ->
    gleam@result:'try'(
        delete_session(Config, Req),
        fun(_) ->
            gleam@result:map(
                (erlang:element(4, erlang:element(4, Config)))(New_session),
                fun(_) ->
                    internal@utils:set_session_cookie(
                        erlang:element(3, Config),
                        Res,
                        Req,
                        New_session
                    )
                end
            )
        end
    ).

-spec middleware(
    session_config(),
    gleam@http@request:request(wisp@internal:connection()),
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()))
) -> gleam@http@response:response(wisp:body()).
middleware(Config, Req, Handle_request) ->
    case internal@utils:get_session_id(erlang:element(3, Config), Req) of
        {ok, _} ->
            Handle_request(Req);

        {error, _} ->
            Session = begin
                _pipe = wisp_kv_sessions@session:builder(),
                _pipe@1 = wisp_kv_sessions@session:with_expiry(
                    _pipe,
                    erlang:element(2, Config)
                ),
                wisp_kv_sessions@session:build(_pipe@1)
            end,
            _ = erlang:element(4, erlang:element(4, Config)),
            Res = begin
                _pipe@2 = internal@utils:inject_session_cookie(
                    erlang:element(3, Config),
                    Req,
                    erlang:element(2, Session),
                    signed
                ),
                Handle_request(_pipe@2)
            end,
            _pipe@3 = Res,
            _pipe@4 = gleam@http@response:get_cookies(_pipe@3),
            _pipe@5 = gleam@list:key_find(_pipe@4, erlang:element(3, Config)),
            (fun(Cookie) -> case Cookie of
                    {ok, _} ->
                        Res;

                    {error, _} ->
                        internal@utils:set_session_cookie(
                            erlang:element(3, Config),
                            Res,
                            Req,
                            Session
                        )
                end end)(_pipe@5)
    end.
