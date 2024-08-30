-module(app).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([middleware/3, handle_request/2, main/0]).

-spec middleware(
    gleam@http@request:request(wisp@internal:connection()),
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body())),
    wisp_kv_sessions:session_config()
) -> gleam@http@response:response(wisp:body()).
middleware(Req, Handle_request, Session_config) ->
    wisp_kv_sessions:middleware(
        Session_config,
        Req,
        fun(Req@1) -> Handle_request(Req@1) end
    ).

-spec get_value_page(
    gleam@http@request:request(wisp@internal:connection()),
    wisp_kv_sessions:session_config()
) -> gleam@http@response:response(wisp:body()).
get_value_page(Req, Session_config) ->
    _assert_subject = wisp_kv_sessions:get(
        Session_config,
        Req,
        <<"test_key"/utf8>>,
        fun gleam@dynamic:string/1
    ),
    {ok, Key} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"app"/utf8>>,
                        function => <<"get_value_page"/utf8>>,
                        line => 64})
    end,
    case Key of
        {some, K} ->
            wisp:html_response(gleam@string_builder:from_string(K), 200);

        none ->
            wisp:html_response(
                gleam@string_builder:from_string(
                    <<"No value set. Go to /set to set a value"/utf8>>
                ),
                200
            )
    end.

-spec set_value_page(
    gleam@http@request:request(wisp@internal:connection()),
    wisp_kv_sessions:session_config()
) -> gleam@http@response:response(wisp:body()).
set_value_page(Req, Session_config) ->
    _ = wisp_kv_sessions:set(
        Session_config,
        Req,
        <<"test_key"/utf8>>,
        <<"something"/utf8>>,
        fun gleam@json:string/1
    ),
    wisp:redirect(<<"/"/utf8>>).

-spec handle_request(
    gleam@http@request:request(wisp@internal:connection()),
    wisp_kv_sessions:session_config()
) -> gleam@http@response:response(wisp:body()).
handle_request(Req, Session_config) ->
    middleware(
        Req,
        fun(Req@1) -> case fun gleam@http@request:path_segments/1(Req@1) of
                [] ->
                    get_value_page(Req@1, Session_config);

                [<<"set"/utf8>>] ->
                    set_value_page(Req@1, Session_config);

                _ ->
                    wisp:not_found()
            end end,
        Session_config
    ).

-spec main() -> {ok, nil} | {error, internal@session_error:session_error()}.
main() ->
    gleam@result:map(
        memory_store:try_create_session_store(),
        fun(Memory_store) ->
            Session_config = {session_config,
                {expire_in, 60 * 60},
                <<"SESSION_COOKIE"/utf8>>,
                Memory_store},
            Secret_key_base = wisp:random_string(64),
            _assert_subject = begin
                _pipe = wisp@wisp_mist:handler(
                    fun(_capture) ->
                        handle_request(_capture, Session_config)
                    end,
                    Secret_key_base
                ),
                _pipe@1 = mist:new(_pipe),
                _pipe@2 = mist:port(_pipe@1, 8000),
                mist:start_http(_pipe@2)
            end,
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"app"/utf8>>,
                                function => <<"main"/utf8>>,
                                line => 29})
            end,
            gleam_erlang_ffi:sleep_forever()
        end
    ).
