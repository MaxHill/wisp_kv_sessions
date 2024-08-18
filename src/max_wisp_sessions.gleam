import birl
import birl/duration
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
import internal/session_id
import wisp

const cookie_name = "SESSION_COOKIE"

pub type Key =
  String

pub type SessionId =
  session_id.SessionId

// Expiry
pub type Expiry {
  ExpireAt(birl.Time)
  ExpireIn(Int)
}

fn expiry_to_date(expiry: Expiry) {
  case expiry {
    ExpireAt(time) -> time
    ExpireIn(seconds) -> {
      birl.now()
      |> birl.add(duration.seconds(seconds))
    }
  }
}

pub opaque type Session {
  Session(
    id: session_id.SessionId,
    expires_at: birl.Time,
    data: Dict(String, Json),
  )
}

pub fn session_new_from_id(id: session_id.SessionId, expiry: Expiry) {
  Session(id: id, expires_at: expiry_to_date(expiry), data: dict.new())
}

pub fn session_new(expiry: Expiry) {
  Session(
    id: session_id.generate(),
    expires_at: expiry_to_date(expiry),
    data: dict.new(),
  )
}

pub fn session_set_key_value(session, key, data) {
  Session(..session, data: dict.insert(session.data, key, data))
}

pub fn session_get(session: Session, key: Key) {
  dict.get(session.data, key)
}

pub type SessionError {
  UnknownError
  DecodeError
  DbSetupError
  NoSessionCookieError
}

/// Session store is the datalayer implementation used
/// when calling session methods
///
pub type SessionStore {
  SessionStore(
    default_expiry: Int,
    get_session: fn(session_id.SessionId) -> Session,
    get: fn(session_id.SessionId, Key) -> option.Option(json.Json),
    set: fn(session_id.SessionId, Key, json.Json) -> Result(Nil, SessionError),
    delete: fn(session_id.SessionId) -> Result(Nil, SessionError),
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
            set_session_cookie(
              res,
              req,
              session_id.SessionId(session_id),
              60 * 60,
            )
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
  |> result.map(session_id.SessionId)
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
  session_id: session_id.SessionId,
  expires_in: Int,
) {
  wisp.set_cookie(
    response,
    req,
    cookie_name,
    session_id.to_string(session_id),
    wisp.Signed,
    expires_in,
  )
}
