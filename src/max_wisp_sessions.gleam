import birl
import gleam/bit_array
import gleam/crypto
import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder}
import gleam/http/request
import gleam/http/response
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import wisp

const cookie_name = "SESSION_COOKIE"

pub type SessionId =
  String

pub type Key =
  String

pub type Session {
  Session(id: SessionId, expiry: birl.Time, data: Dict(String, Json))
}

pub type SessionError {
  UnknownError
  DecodeError
  DbSetupError
  NoSessionCookieError
}

pub type SessionStore {
  SessionStore(
    get_session: fn(SessionId) -> Session,
    get: fn(SessionId, Key) -> Option(Json),
    set: fn(SessionId, Key, Json) -> Result(Nil, SessionError),
    delete: fn(SessionId) -> Result(Nil, SessionError),
  )
}

/// Get data from session by key
///
pub fn get(
  store: SessionStore,
  req: wisp.Request,
  key: Key,
  decoder: Decoder(data),
) -> Result(Option(data), SessionError) {
  use session_id <- result.try(get_session_id(req))
  case store.get(session_id, key) {
    option.Some(data) -> {
      json.decode(from: json.to_string(data), using: decoder)
      |> result.replace_error(DecodeError)
      |> result.map(fn(v) { option.Some(v) })
    }
    option.None -> Ok(option.None)
  }
}

/// Set data in session by key
///
pub fn set(
  store: SessionStore,
  req: wisp.Request,
  key: Key,
  data: data,
  encoder: fn(data) -> Json,
) {
  use session_id <- result.try(get_session_id(req))
  let json_data = encoder(data)
  use _ <- result.map(store.set(session_id, key, json_data))
  data
}

/// Remove session
/// Usage: 
/// ```gleam
/// sessions.delete(store, req)
/// ```
pub fn delete(store: SessionStore, req: wisp.Request) {
  use session_id <- result.try(get_session_id(req))
  store.delete(session_id)
}

// Middleware

/// Create a session if no session exists to make sure there 
/// always is a session_id to store data towards
/// Usage: 
/// ```gleam
/// use <- sessions.middleware(req)
/// ```
pub fn middleware(
  req: wisp.Request,
  handle_request: fn(wisp.Request) -> wisp.Response,
) -> wisp.Response {
  case get_session_id(req) {
    Ok(_) -> handle_request(req)
    Error(_) -> {
      let session_id = "Generate_me"
      let res =
        inject_session_cookie(req, session_id, wisp.Signed)
        |> handle_request

      // Only set the cookie if it has not already been set in the handler
      res
      |> response.get_cookies
      |> list.key_find(cookie_name)
      |> fn(cookie) {
        case cookie {
          Ok(_) -> res
          Error(_) -> {
            io.print("Should get here")
            set_session_cookie(res, req, session_id, 60 * 60)
          }
        }
      }
    }
  }
}

// Helpers
fn get_session_id(req: wisp.Request) {
  wisp.get_cookie(req, cookie_name, wisp.Signed)
  |> result.replace_error(NoSessionCookieError)
}

/// Inject a cookie into a request
/// This will NOT be persisted between requests
pub fn inject_session_cookie(
  request req: wisp.Request,
  value value: String,
  security security: wisp.Security,
) -> wisp.Request {
  let value = case security {
    wisp.PlainText -> bit_array.base64_encode(<<value:utf8>>, False)
    wisp.Signed -> wisp.sign_message(req, <<value:utf8>>, crypto.Sha512)
  }
  req
  |> request.set_cookie(cookie_name, value)
}

pub fn set_session_cookie(
  response: wisp.Response,
  req: wisp.Request,
  session_id: SessionId,
  expires_in: Int,
) {
  wisp.set_cookie(
    response,
    req,
    cookie_name,
    session_id,
    wisp.Signed,
    expires_in,
  )
}
