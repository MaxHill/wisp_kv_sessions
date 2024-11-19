import gleam/dict
import gleam/dynamic.{type Decoder}
import gleam/http/response
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import wisp
import wisp_kv_sessions/internal/utils
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config

pub type CurrentSession {
  CurrentSession(req: wisp.Request, config: session_config.Config)
}

/// Try to get the session from the store. 
/// If it does not exist create a new one.
///
pub fn get_session(current_session: CurrentSession) {
  use session_id <- result.try(utils.get_session_id(
    current_session.config.cookie_name,
    current_session.req,
  ))
  use maybe_session <- result.try(get_session_with_cache(
    session_id,
    current_session.config,
    True,
  ))

  case maybe_session {
    option.Some(session) -> {
      case utils.is_session_expired(session) {
        // cookie should expire at the same time the session
        // does so we should not get expired sessions
        True -> Error(session.SessionExpiredError)
        False -> Ok(session)
      }
    }
    option.None -> {
      let session =
        session.builder()
        |> session.with_id(session_id)
        |> session.with_expiry(current_session.config.default_expiry)
        |> session.build

      save_session(current_session.config, session)
    }
  }
}

fn get_session_with_cache(
  session_id: session.SessionId,
  config: session_config.Config,
  cache: Bool,
) {
  case cache, config.cache {
    True, option.Some(cache) -> {
      case cache.get_session(session_id) {
        Ok(option.Some(session)) -> Ok(option.Some(session))
        _ -> get_session_with_cache(session_id, config, False)
      }
    }
    _, _ -> {
      config.store.get_session(session_id)
    }
  }
}

fn save_session(config: session_config.Config, session: session.Session) {
  config.cache
  |> option.map(fn(cache) { cache.save_session(session) })

  config.store.save_session(session)
}

/// Remove session
/// Usage: 
/// ```gleam
/// sessions.delete(store, req)
/// ```
pub fn delete_session(current_session: CurrentSession) {
  let CurrentSession(config: config, req: req) = current_session
  use session_id <- result.try(utils.get_session_id(config.cookie_name, req))
  config.cache
  |> option.map(fn(cache) { cache.delete_session(session_id) })
  config.store.delete_session(session_id)
}

pub type SessionKey(data) {
  SessionKey(
    current_session: CurrentSession,
    key: String,
    decode: Decoder(data),
    encode: fn(data) -> String,
  )
}

pub fn key(current_session: CurrentSession, key: String) {
  SessionKey(
    current_session:,
    key:,
    decode: dynamic.string,
    encode: fn(str: String) { json.string(str) |> json.to_string },
  )
}

pub fn with_codec(
  session_key: SessionKey(a),
  decoder decode: Decoder(data),
  encoder encode: fn(data) -> String,
) -> SessionKey(data) {
  SessionKey(
    current_session: session_key.current_session,
    key: session_key.key,
    decode:,
    encode:,
  )
}

/// Get data from session by key
///
pub fn get(entry: SessionKey(data)) {
  use session <- result.try(get_session(entry.current_session))
  case dict.get(session.data, entry.key) |> option.from_result {
    option.None -> Ok(option.None)
    option.Some(data) -> {
      json.decode(from: data, using: entry.decode)
      |> result.replace_error(session.DecodeError)
      |> result.map(fn(d) { option.Some(d) })
    }
  }
}

/// Set data in session by key
///
pub fn set(entry: SessionKey(data), data: data) {
  use session <- result.try(get_session(entry.current_session))

  let session =
    session.builder_from(session)
    |> session.with_entry(entry.key, entry.encode(data))
    |> session.build
  use _ <- result.map(save_session(entry.current_session.config, session))
  data
}

/// Delete key from session
///
pub fn delete(current_session: CurrentSession, key: session.Key) {
  let CurrentSession(config: config, req: _req) = current_session
  use session <- result.try(get_session(current_session))
  save_session(
    config,
    session.Session(..session, data: dict.delete(session.data, key)),
  )
}

/// Replace the session with a new one
/// Usage:
/// ```gleam
/// wisp.ok() |> replace_session(new_session)
/// ```
///
pub fn replace_session(
  current_session: CurrentSession,
  res: wisp.Response,
  new_session: session.Session,
) {
  let CurrentSession(config: config, req: req) = current_session
  use _ <- result.try(delete_session(current_session))
  use _ <- result.map(save_session(config, new_session))
  utils.set_session_cookie(config.cookie_name, res, req, new_session)
}

// Middleware

/// Create a session if no session exists to make sure there 
/// always is a session_id to store data towards
/// Usage: 
/// ```gleam
/// use <- sessions.middleware(current_session)
/// ```
pub fn middleware(
  req: wisp.Request,
  config: session_config.Config,
  handle_request: fn(wisp.Request) -> wisp.Response,
) {
  case utils.get_session_id(config.cookie_name, req) {
    Ok(_) -> handle_request(req)
    Error(_) -> {
      let session =
        session.builder()
        |> session.with_expiry(config.default_expiry)
        |> session.build()

      // Try to save the session and fail silently
      let _ = config.store.save_session(session)

      let res =
        utils.inject_session_cookie(
          config.cookie_name,
          req,
          session.id,
          wisp.Signed,
        )
        |> handle_request

      // Only set the cookie if it has not already been set in the handler
      res
      |> response.get_cookies
      |> list.key_find(config.cookie_name)
      |> fn(cookie) {
        case cookie {
          Ok(_) -> res
          Error(_) -> {
            utils.set_session_cookie(config.cookie_name, res, req, session)
          }
        }
      }
    }
  }
}
