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
import wisp

// Expiry
pub type Expiry {
  ExpireAt(birl.Time)
  ExpireIn(Int)
}

// Session type
type Key =
  String

pub type SessionId =
  session_id.SessionId

pub type Session {
  Session(
    id: session_id.SessionId,
    expires_at: birl.Time,
    data: dict.Dict(Key, json.Json),
  )
}

pub fn session_new_with_id(id: session_id.SessionId, expiry: Expiry) {
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

pub fn session_expires_at(session: Session) {
  session.expires_at
}

// Errors
pub type SessionError {
  UnknownError
  DecodeError
  DbSetupError
  DbError
  NoSessionCookieError
}

// Config
pub type SessionConfig {
  SessionConfig(
    default_expiry: Expiry,
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
    get_session: fn(session_id.SessionId) -> option.Option(Session),
    create_session: fn(Session) -> Result(Session, SessionError),
    get: fn(session_id.SessionId, Key) -> option.Option(json.Json),
    set: fn(session_id.SessionId, Key, json.Json) -> Result(Nil, SessionError),
    delete: fn(session_id.SessionId) -> Result(Nil, SessionError),
  )
}

/// Try to get the session from the store. 
/// If it does not exist create a new one.
///
pub fn get_session(config: SessionConfig, req: wisp.Request) {
  use session_id <- result.try(get_session_id(config, req))
  let maybe_session = config.store.get_session(session_id)
  case maybe_session {
    option.Some(session) -> Ok(session)
    option.None ->
      config.store.create_session(session_new_with_id(
        session_id,
        config.default_expiry,
      ))
  }
}

/// Get data from session by key
///
pub fn get(
  config: SessionConfig,
  req: wisp.Request,
  key: Key,
  decoder: Decoder(data),
) -> Result(option.Option(data), SessionError) {
  use session_id <- result.try(get_session_id(config, req))
  case config.store.get(session_id, key) {
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
  config: SessionConfig,
  req: wisp.Request,
  key: Key,
  data: data,
  encoder: fn(data) -> json.Json,
) {
  use session_id <- result.try(get_session_id(config, req))
  let json_data = encoder(data)
  use _ <- result.map(config.store.set(session_id, key, json_data))
  data
}

/// Remove session
/// Usage: 
/// ```gleam
/// sessions.delete(store, req)
/// ```
pub fn delete(config: SessionConfig, req: wisp.Request) {
  use session_id <- result.try(get_session_id(config, req))
  config.store.delete(session_id)
}

// Middleware

/// Create a session if no session exists to make sure there 
/// always is a session_id to store data towards
/// Usage: 
/// ```gleam
/// use <- sessions.middleware(req)
/// ```
pub fn create_middleware(config: SessionConfig) {
  fn(req: wisp.Request, handle_request: fn(wisp.Request) -> wisp.Response) -> wisp.Response {
    case get_session_id(config, req) {
      Ok(_) -> handle_request(req)
      Error(_) -> {
        let session_id = session_id.generate()
        let res =
          inject_session_cookie(config, req, session_id, wisp.Signed)
          |> handle_request

        // Only set the cookie if it has not already been set in the handler
        res
        |> response.get_cookies
        |> list.key_find(config.cookie_name)
        |> fn(cookie) {
          case cookie {
            Ok(_) -> res
            Error(_) -> {
              set_session_cookie(
                config,
                res,
                req,
                session_id,
                seconds_from_now(config.default_expiry),
              )
            }
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
  value session_id: SessionId,
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
  session_id: session_id.SessionId,
  expires_in: Int,
) {
  wisp.set_cookie(
    response,
    req,
    config.cookie_name,
    session_id.to_string(session_id),
    wisp.Signed,
    expires_in,
  )
}

fn get_session_id(config: SessionConfig, req: wisp.Request) {
  wisp.get_cookie(req, config.cookie_name, wisp.Signed)
  |> result.replace_error(NoSessionCookieError)
  |> result.map(session_id.SessionId)
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

fn seconds_from_now(time: Expiry) {
  expiry_to_date(time)
  |> birl.difference(birl.now())
  |> duration.blur_to(duration.Second)
}
