-record(session_builder, {
    id :: internal@session_id:session_id(),
    expiry :: gleam@option:option(wisp_kv_sessions@session:expiry()),
    data :: gleam@dict:dict(binary(), gleam@json:json())
}).
