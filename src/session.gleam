import birl
import birl/duration
import gleam/dict
import gleam/json
import gleam/option
import internal/session_id

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

pub fn session_get(session: Session, key: Key) {
  dict.get(session.data, key)
}

pub fn session_expires_at(session: Session) {
  session.expires_at
}

// Expiry
pub type Expiry {
  ExpireAt(birl.Time)
  ExpireIn(Int)
}

// Session Builder
//---------------------
pub type SessionBuilder {
  SessionBuilder(
    id: session_id.SessionId,
    expiry: option.Option(Expiry),
    data: dict.Dict(Key, json.Json),
  )
}

pub fn builder_from(session: Session) {
  SessionBuilder(
    id: session.id,
    expiry: option.Some(ExpireAt(session.expires_at)),
    data: session.data,
  )
}

pub fn builder() {
  SessionBuilder(
    id: session_id.generate(),
    expiry: option.None,
    data: dict.new(),
  )
}

pub fn with_id(session: SessionBuilder, id: session_id.SessionId) {
  SessionBuilder(..session, id: id)
}

pub fn with_id_string(session: SessionBuilder, id: String) {
  SessionBuilder(..session, id: session_id.SessionId(id))
}

pub fn with_expiry(session: SessionBuilder, expiry: Expiry) {
  SessionBuilder(..session, expiry: option.Some(expiry))
}

pub fn with_data(session: SessionBuilder, data: dict.Dict(Key, json.Json)) {
  SessionBuilder(..session, data: data)
}

pub fn set_key_value(session, key, data) {
  SessionBuilder(..session, data: dict.insert(session.data, key, data))
}

pub fn build(session: SessionBuilder) {
  let expiry = option.unwrap(session.expiry, ExpireIn(60 * 60))
  Session(
    id: session.id,
    expires_at: expiry_to_date(expiry),
    data: session.data,
  )
}

pub fn expiry_to_date(expiry: Expiry) {
  case expiry {
    ExpireAt(time) -> time
    ExpireIn(seconds) -> {
      birl.now()
      |> birl.add(duration.seconds(seconds))
    }
  }
}
