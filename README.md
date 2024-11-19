# wisp_kv_sessions

[![Package Version](https://img.shields.io/hexpm/v/wisp_kv_sessions)](https://hex.pm/packages/wisp_kv_sessions)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/wisp_kv_sessions/)


# Overview
wisp_kv_sessions is a key-value session management library for [Wisp](https://gleam-wisp.github.io/wisp/), inspired by the Rust crate [tower sessions](https://docs.rs/tower-sessions/latest/tower_sessions/#). This library allows you to manage user sessions with ease, storing session data in a simple key-value store. 

# Example Usage
An minimal example usage is available in `./example/app.gleam`.

```gleam
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
```

## Running the Example

To start the server, cd into /example and run `gleam run`.

Visit (http://localhost:8000) in your browser. The page should display "No value set. Go to /set to set a value."


Navigate to (http://localhost:8000/set). You will be redirected back to the main page, which will now display "something".

# Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
just watch-test # Run test and reload on file changes
```

# Caching

A session config can be passed an optional cache parameter that is a `SessionStore`
wrapped in an `option.Option`. If the cache is `option.Some` sessions will be 
fetched from the cache. If the data is not in the cache the `store` will be 
tried.

Session data will be automatically added and removed from the cache.

# SessionStore 
There are different places you may want to store sessions such as 
postgres/ETS/sqlite. The way you integrate with them is through storage adapters 
that implement the type SessionStore. You can use one of the prebuild storage 
adapters from down below, or implement a new one if the one you
are looking for does not exist.

For an example implementation, see `./src/wisp_kv_sessions/ets_adapter.gleam`.

## Included SessionStore adapters

### actor_adapter
The actor_adapter driver is suitable for development and testing purposes, 
but not recommended for production due to its non-concurrent nature. 
Internally, it uses an [actor](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html), 
which may become a bottleneck under heavy loads.

*Usage Example:*

```gleam
import wisp_kv_sessions/actor_adapter

use session_store <- result.map(actor_adapter.new())

// ...
```
See `./example/src/app.gleam` for full example

### postgres_adapter
Also included is the postgres_adapter, that allows you to use postgres as 
the storage implementation

```gleam
import wisp_kv_sessions/postgres_adapter

let db = 
  pog.default_config()
  |> pog.connect()

// Migrate
use _ <- result.try(postgres_adapter.migrate_up(conn))

// Setup session_store
use session_store <- result.map(postgres_adapter.new(conn))

//...
```


### ets_adapter

The ets_adapter uses [Erlang Term Storage](https://www.erlang.org/doc/apps/stdlib/ets.html) 
and [carpenter](https://hexdocs.pm/carpenter/) to store session information.
*This will NOT be persistant after restarts*. But is a good option for caching.

```gleam
import wisp_kv_sessions/ets_adapter

// Setup session_store
use session_store <- result.map(ets_adapter.new(conn))

//...
```
