-module(internal@session_error).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([session_error/0]).

-type session_error() :: unknown_error |
    decode_error |
    db_setup_error |
    db_error |
    no_session_cookie_error.


