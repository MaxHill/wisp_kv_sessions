-record(session, {
    id :: internal@session_id:session_id(),
    expires_at :: birl:time(),
    data :: gleam@dict:dict(binary(), gleam@json:json())
}).
