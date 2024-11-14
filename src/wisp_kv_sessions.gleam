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

/// Try to get the session from the store. 
/// If it does not exist create a new one.
///
pub fn get_session(config: session_config.Config, req: wisp.Request) {
  use session_id <- result.try(utils.get_session_id(config.cookie_name, req))
  use maybe_session <- result.try(get_session_with_cache(
    session_id,
    config,
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
        |> session.with_expiry(config.default_expiry)
        |> session.build

      save_session(config, session)
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
pub fn delete_session(config: session_config.Config, req: wisp.Request) {
  use session_id <- result.try(utils.get_session_id(config.cookie_name, req))
  config.cache
  |> option.map(fn(cache) { cache.delete_session(session_id) })
  config.store.delete_session(session_id)
}

/// Get data from session by key
///
pub fn get(
  config: session_config.Config,
  req: wisp.Request,
  key: session.Key,
  decoder: Decoder(data),
) {
  use session <- result.try(get_session(config, req))
  case dict.get(session.data, key) |> option.from_result {
    option.None -> Ok(option.None)
    option.Some(data) -> {
      json.decode(from: data, using: decoder)
      |> result.replace_error(session.DecodeError)
      |> result.map(fn(d) { option.Some(d) })
    }
  }
}

/// Set data in session by key
///
pub fn set(
  config: session_config.Config,
  req: wisp.Request,
  key: session.Key,
  data: data,
  encoder: fn(data) -> String,
) {
  use session <- result.try(get_session(config, req))
  let json_data = encoder(data)
  let new_session =
    session.builder_from(session)
    |> session.with_entry(key, json_data)
    |> session.build
  use _ <- result.map(save_session(config, new_session))
  data
}

/// Delete key from session
///
pub fn delete(
  config: session_config.Config,
  req: wisp.Request,
  key: session.Key,
) {
  use session <- result.try(get_session(config, req))
  save_session(
    config,
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
  config: session_config.Config,
  res: wisp.Response,
  req: wisp.Request,
  new_session: session.Session,
) {
  use _ <- result.try(delete_session(config, req))
  use _ <- result.map(save_session(config, new_session))
  utils.set_session_cookie(config.cookie_name, res, req, new_session)
}

// Middleware

/// Create a session if no session exists to make sure there 
/// always is a session_id to store data towards
/// Usage: 
/// ```gleam
/// use <- sessions.middleware(config, req)
/// ```
pub fn middleware(
  config: session_config.Config,
  req: wisp.Request,
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
