-record(session_config, {
    default_expiry :: wisp_kv_sessions@session:expiry(),
    cookie_name :: binary(),
    store :: wisp_kv_sessions:session_store()
}).
