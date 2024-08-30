import gleam/dynamic
import gleam/erlang/process
import gleam/json
import gleam/option
import gleam/result
import gleam/string_builder
import memory_store
import mist
import wisp
import wisp/wisp_mist
import wisp_kv_sessions
import wisp_kv_sessions/session

pub fn main() {
  // Setup session_store
  use memory_store <- result.map(memory_store.try_create_session_store())

  // Create session config
  let session_config =
    wisp_kv_sessions.SessionConfig(
      default_expiry: session.ExpireIn(60 * 60),
      cookie_name: "SESSION_COOKIE",
      store: memory_store,
    )

  let secret_key_base = wisp.random_string(64)

  // Start the Mist web server.
  let assert Ok(_) =
    wisp_mist.handler(handle_request(_, session_config), secret_key_base)
    |> mist.new
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}

pub fn handle_request(req: wisp.Request, session_config) -> wisp.Response {
  use req <- middleware(req, _, session_config)

  case wisp.path_segments(req) {
    [] -> get_value_page(req, session_config)
    ["set"] -> set_value_page(req, session_config)
    _ -> wisp.not_found()
  }
}

pub fn middleware(
  req: wisp.Request,
  handle_request: fn(wisp.Request) -> wisp.Response,
  session_config: wisp_kv_sessions.SessionConfig,
) -> wisp.Response {
  // Add the middleware to handle sessions
  use req <- wisp_kv_sessions.middleware(session_config, req, _)

  handle_request(req)
}

fn get_value_page(
  req: wisp.Request,
  session_config: wisp_kv_sessions.SessionConfig,
) -> wisp.Response {
  // Read value to the session
  let assert Ok(key) =
    wisp_kv_sessions.get(session_config, req, "test_key", dynamic.string)

  case key {
    option.Some(k) -> {
      wisp.html_response(string_builder.from_string(k), 200)
    }
    option.None -> {
      wisp.html_response(
        string_builder.from_string("No value set. Go to /set to set a value"),
        200,
      )
    }
  }
}

fn set_value_page(
  req: wisp.Request,
  session_config: wisp_kv_sessions.SessionConfig,
) -> wisp.Response {
  // Set value to the session
  let _ =
    wisp_kv_sessions.set(
      session_config,
      req,
      "test_key",
      "something",
      json.string,
    )
  wisp.redirect("/")
}
