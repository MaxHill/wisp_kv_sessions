import birl
import birl/duration
import gleam/bit_array
import gleam/crypto
import gleam/dict
import gleam/dynamic.{type Decoder}
import gleam/http/request
import gleam/http/response
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import internal/session_id
import session
import wisp

// Config
//---------------------
pub type SessionConfig {
  SessionConfig(
    default_expiry: session.Expiry,
    cookie_name: String,
    store: SessionStore,
  )
}

/// Session store is the datalayer implementation used
/// when calling session methods
///
pub type SessionStore {
  SessionStore(
    default_expiry: Int,
    get_session: fn(session_id.SessionId) ->
      Result(option.Option(session.Session), SessionError),
    save_session: fn(session.Session) -> Result(session.Session, SessionError),
    delete_session: fn(session_id.SessionId) -> Result(Nil, SessionError),
  )
}

// Errors
//---------------------
pub type SessionError {
  UnknownError
  DecodeError
  DbSetupError
  DbError
  NoSessionCookieError
}

/// Try to get the session from the store. 
/// If it does not exist create a new one.
///
pub fn get_session(config: SessionConfig, req: wisp.Request) {
  use session_id <- result.try(get_session_id(config, req))
  use maybe_session <- result.try(config.store.get_session(session_id))
  case maybe_session {
    option.Some(session) -> Ok(session)
    option.None ->
      session.builder()
      |> session.with_id(session_id)
      |> session.with_expiry(config.default_expiry)
      |> session.build
      |> config.store.save_session
  }
}

/// Remove session
/// Usage: 
/// ```gleam
/// sessions.delete(store, req)
/// ```
pub fn delete_session(config: SessionConfig, req: wisp.Request) {
  use session_id <- result.try(get_session_id(config, req))
  config.store.delete_session(session_id)
}

/// Get data from session by key
///
pub fn get(
  config: SessionConfig,
  req: wisp.Request,
  key: session.Key,
  decoder: Decoder(data),
) {
  use session <- result.try(get_session(config, req))
  case session.session_get(session, key) |> option.from_result {
    option.None -> Ok(option.None)
    option.Some(data) -> {
      json.decode(from: json.to_string(data), using: decoder)
      |> result.replace_error(DecodeError)
      |> result.map(fn(d) { option.Some(d) })
    }
  }
}

/// Set data in session by key
///
pub fn set(
  config: SessionConfig,
  req: wisp.Request,
  key: session.Key,
  data: data,
  encoder: fn(data) -> json.Json,
) {
  use session <- result.try(get_session(config, req))
  let json_data = encoder(data)
  let new_session =
    session.builder_from(session)
    |> session.set_key_value(key, json_data)
    |> session.build
  use _ <- result.map(config.store.save_session(new_session))
  data
}

/// Delete key from session
///
pub fn delete(config: SessionConfig, req: wisp.Request, key: session.Key) {
  use session <- result.try(get_session(config, req))
  config.store.save_session(
    session.Session(..session, data: dict.delete(session.data, key)),
  )
}

/// Replace the session with a new one
/// Usage:
/// ```gleam
/// wisp.ok() |> replace_session(session_config, req)
/// ```
///
pub fn replace_session(
  config: SessionConfig,
  res: wisp.Response,
  req: wisp.Request,
  new_session: session.Session,
) {
  use _ <- result.try(delete_session(config, req))
  use _ <- result.map(config.store.save_session(new_session))
  set_session_cookie(config, res, req, new_session)
}

// Middleware

/// Create a session if no session exists to make sure there 
/// always is a session_id to store data towards
/// Usage: 
/// ```gleam
/// use <- sessions.middleware(config, req)
/// ```
pub fn middleware(
  config: SessionConfig,
  req: wisp.Request,
  handle_request: fn(wisp.Request) -> wisp.Response,
) {
  case get_session_id(config, req) {
    Ok(_) -> handle_request(req)
    Error(_) -> {
      let session =
        session.builder()
        |> session.with_expiry(config.default_expiry)
        |> session.build()

      // Try to save the session and fail silently
      let _ = config.store.save_session

      let res =
        inject_session_cookie(config, req, session.id, wisp.Signed)
        |> handle_request

      // Only set the cookie if it has not already been set in the handler
      res
      |> response.get_cookies
      |> list.key_find(config.cookie_name)
      |> fn(cookie) {
        case cookie {
          Ok(_) -> res
          Error(_) -> {
            set_session_cookie(config, res, req, session)
          }
        }
      }
    }
  }
}

/// Inject a cookie into a request
/// This will NOT be persisted between requests
pub fn inject_session_cookie(
  config config: SessionConfig,
  request req: wisp.Request,
  value session_id: session_id.SessionId,
  security security: wisp.Security,
) -> wisp.Request {
  let value = session_id.to_string(session_id)
  let value = case security {
    wisp.PlainText -> bit_array.base64_encode(<<value:utf8>>, False)
    wisp.Signed -> wisp.sign_message(req, <<value:utf8>>, crypto.Sha512)
  }
  req
  |> request.set_cookie(config.cookie_name, value)
}

// Helpers
pub fn set_session_cookie(
  config: SessionConfig,
  response: wisp.Response,
  req: wisp.Request,
  session: session.Session,
) {
  wisp.set_cookie(
    response,
    req,
    config.cookie_name,
    session_id.to_string(session.id),
    wisp.Signed,
    seconds_from_now(session.expires_at),
  )
}

fn get_session_id(config: SessionConfig, req: wisp.Request) {
  wisp.get_cookie(req, config.cookie_name, wisp.Signed)
  |> result.replace_error(NoSessionCookieError)
  |> result.map(session_id.SessionId)
}

fn seconds_from_now(time: birl.Time) {
  time
  |> birl.difference(birl.now())
  |> duration.blur_to(duration.Second)
}
