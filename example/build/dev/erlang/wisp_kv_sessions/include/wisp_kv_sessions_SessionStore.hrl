-record(session_store, {
    default_expiry :: integer(),
    get_session :: fun((internal@session_id:session_id()) -> {ok,
            gleam@option:option(wisp_kv_sessions@session:session())} |
        {error, internal@session_error:session_error()}),
    save_session :: fun((wisp_kv_sessions@session:session()) -> {ok,
            wisp_kv_sessions@session:session()} |
        {error, internal@session_error:session_error()}),
    delete_session :: fun((internal@session_id:session_id()) -> {ok, nil} |
        {error, internal@session_error:session_error()})
}).
