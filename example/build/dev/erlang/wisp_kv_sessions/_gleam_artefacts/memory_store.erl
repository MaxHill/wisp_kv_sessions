-module(memory_store).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([try_create_session_store/0]).
-export_type([message/1]).

-type message(BDU) :: shutdown |
    {get_session,
        gleam@erlang@process:subject({ok, wisp_kv_sessions@session:session()} |
            {error, nil}),
        internal@session_id:session_id()} |
    {set_session,
        gleam@erlang@process:subject(wisp_kv_sessions@session:session()),
        wisp_kv_sessions@session:session()} |
    {delete_session, internal@session_id:session_id()} |
    {gleam_phantom, BDU}.

-spec delete_session(gleam@erlang@process:subject(message(any()))) -> fun((internal@session_id:session_id()) -> {ok,
        nil} |
    {error, any()}).
delete_session(Db) ->
    fun(Session_id) ->
        gleam@erlang@process:send(Db, {delete_session, Session_id}),
        {ok, nil}
    end.

-spec handle_message(
    message(BEK),
    gleam@dict:dict(internal@session_id:session_id(), wisp_kv_sessions@session:session())
) -> gleam@otp@actor:next(message(BEK), gleam@dict:dict(internal@session_id:session_id(), wisp_kv_sessions@session:session())).
handle_message(Message, Db) ->
    case Message of
        shutdown ->
            {stop, normal};

        {get_session, Client, Session_id} ->
            Session = gleam@dict:get(Db, Session_id),
            gleam@erlang@process:send(Client, Session),
            gleam@otp@actor:continue(Db);

        {set_session, Client@1, Session@1} ->
            gleam@erlang@process:send(Client@1, Session@1),
            gleam@otp@actor:continue(
                gleam@dict:insert(Db, erlang:element(2, Session@1), Session@1)
            );

        {delete_session, Session_id@1} ->
            gleam@otp@actor:continue(gleam@dict:delete(Db, Session_id@1))
    end.

-spec get_session(gleam@erlang@process:subject(message(any()))) -> fun((internal@session_id:session_id()) -> {ok,
        gleam@option:option(wisp_kv_sessions@session:session())} |
    {error, any()}).
get_session(Db) ->
    fun(Session_id) ->
        {ok,
            begin
                _pipe = gleam@erlang@process:call(
                    Db,
                    fun(Client) -> {get_session, Client, Session_id} end,
                    3000
                ),
                _pipe@1 = gleam@result:map(
                    _pipe,
                    fun(Session) -> {some, Session} end
                ),
                _pipe@2 = gleam@option:from_result(_pipe@1),
                gleam@option:flatten(_pipe@2)
            end}
    end.

-spec save_session(gleam@erlang@process:subject(message(any()))) -> fun((wisp_kv_sessions@session:session()) -> {ok,
        wisp_kv_sessions@session:session()} |
    {error, any()}).
save_session(Db) ->
    fun(Session) ->
        {ok,
            gleam@erlang@process:call(
                Db,
                fun(Client) -> {set_session, Client, Session} end,
                3000
            )}
    end.

-spec try_create_session_store() -> {ok, wisp_kv_sessions:session_store()} |
    {error, internal@session_error:session_error()}.
try_create_session_store() ->
    gleam@result:map(
        begin
            _pipe = gleam@otp@actor:start(
                gleam@dict:new(),
                fun handle_message/2
            ),
            gleam@result:replace_error(_pipe, db_setup_error)
        end,
        fun(Db) ->
            {session_store,
                60 * 60,
                get_session(Db),
                save_session(Db),
                delete_session(Db)}
        end
    ).
