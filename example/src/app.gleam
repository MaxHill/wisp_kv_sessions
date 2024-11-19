import gleam/erlang/process
import gleam/option
import gleam/result
import gleam/string_builder
import mist
import wisp
import wisp/wisp_mist
import wisp_kv_sessions
import wisp_kv_sessions/actor_adapter
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config

pub fn main() {
  // Setup session_adapter
  use store <- result.map(actor_adapter.new())
  use cache_store <- result.map(actor_adapter.new())

  // Create session config
  let session_config =
    session_config.Config(
      default_expiry: session.ExpireIn(60 * 60),
      cookie_name: "SESSION_COOKIE",
      store:,
      cache: option.Some(cache_store),
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
  // Run the middleware and construct current_session
  use req <- wisp_kv_sessions.middleware(req, session_config)
  let current_session = wisp_kv_sessions.CurrentSession(req, session_config)

  case wisp.path_segments(req) {
    [] -> get_value_page(req, current_session)
    ["set"] -> set_value_page(req, current_session)
    _ -> wisp.not_found()
  }
}

fn get_value_page(
  _req: wisp.Request,
  session: wisp_kv_sessions.CurrentSession,
) -> wisp.Response {
  // Read value to the session
  let assert Ok(key) =
    session
    |> wisp_kv_sessions.key("test_key")
    |> wisp_kv_sessions.get()

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
  _req: wisp.Request,
  session: wisp_kv_sessions.CurrentSession,
) -> wisp.Response {
  // Set value to the session
  let _ =
    session
    |> wisp_kv_sessions.key("test_key")
    |> wisp_kv_sessions.set("something")

  wisp.redirect("/")
}
