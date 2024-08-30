{application, wisp_kv_sessions, [
    {vsn, "1.0.0"},
    {applications, [birl,
                    gleam_crypto,
                    gleam_erlang,
                    gleam_http,
                    gleam_json,
                    gleam_otp,
                    gleam_stdlib,
                    wisp]},
    {description, ""},
    {modules, [internal@utils,
               memory_store,
               wisp_kv_sessions]},
    {registered, []}
]}.
