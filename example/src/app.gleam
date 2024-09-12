import gleam/dynamic
import gleam/erlang/process
import gleam/json
import gleam/option
import gleam/result
import gleam/string_builder
import mist
import wisp
import wisp/wisp_mist
import wisp_kv_sessions
import wisp_kv_sessions/actor_store
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config

pub fn main() {
  // Setup session_store
  use actor_store <- result.map(actor_store.try_create_session_store())

  // Create session config
  let session_config =
    session_config.Config(
      default_expiry: session.ExpireIn(60 * 60),
      cookie_name: "SESSION_COOKIE",
      store: actor_store,
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
  use req <- wisp_kv_sessions.middleware(session_config, req)

  case wisp.path_segments(req) {
    [] -> get_value_page(req, session_config)
    ["set"] -> set_value_page(req, session_config)
    _ -> wisp.not_found()
  }
}

fn get_value_page(
  req: wisp.Request,
  session_config: session_config.Config,
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

// Not nessesary for this simple type 
fn encode_string(str: String) {
  json.string(str) |> json.to_string
}

fn set_value_page(
  req: wisp.Request,
  session_config: session_config.Config,
) -> wisp.Response {
  // Set value to the session
  let _ =
    wisp_kv_sessions.set(
      session_config,
      req,
      "test_key",
      "something",
      encode_string,
    )
  wisp.redirect("/")
}
