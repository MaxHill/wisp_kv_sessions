import gleam/option
import wisp_kv_sessions/session

// Config
//---------------------
pub type Config {
  Config(
    default_expiry: session.Expiry,
    cookie_name: String,
    store: SessionStore,
    cache: option.Option(SessionStore),
  )
}

/// Session store is what the different storages should implement.
/// It is used when saving and getting session
///
pub type SessionStore {
  SessionStore(
    get_session: fn(session.SessionId) ->
      Result(option.Option(session.Session), session.SessionError),
    save_session: fn(session.Session) ->
      Result(session.Session, session.SessionError),
    delete_session: fn(session.SessionId) -> Result(Nil, session.SessionError),
  )
}
