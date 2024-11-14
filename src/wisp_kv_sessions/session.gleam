import birl
import birl/duration
import gleam/dict
import gleam/option
import wisp

// Errors
//---------------------
pub type SessionError {
  UnknownError
  DecodeError
  DbSetupError
  DbErrorInsertError(String)
  DbErrorGetError(String)
  DbErrorDeleteError(String)
  DeserializeError(String)
  SessionExpiredError
  NoSessionError
}

// Session id
//---------------------
pub opaque type SessionId {
  SessionId(String)
}

/// Create a new session id
pub fn generate_id() {
  SessionId(wisp.random_string(200))
}

/// Unwrap session id to a string
pub fn id_to_string(id: SessionId) {
  case id {
    SessionId(id_string) -> id_string
  }
}

/// Unwrap session id to a string
pub fn id_from_string(id: String) {
  SessionId(id)
}

// Session 
//---------------------
type Key =
  String

type JsonString =
  String

pub type Session {
  Session(
    id: SessionId,
    expires_at: #(#(Int, Int, Int), #(Int, Int, Int)),
    data: dict.Dict(Key, JsonString),
  )
}

// Expiry
pub type Expiry {
  ExpireAt(birl.Time)
  ExpireIn(Int)
}

/// Convert expiry type to a date
pub fn expiry_to_date(expiry: Expiry) {
  case expiry {
    ExpireAt(time) -> time
    ExpireIn(seconds) -> {
      birl.now()
      |> birl.add(duration.seconds(seconds))
    }
  }
  |> birl.to_erlang_universal_datetime
}

// Session Builder
//---------------------
pub type SessionBuilder {
  SessionBuilder(
    id: SessionId,
    expiry: option.Option(Expiry),
    data: dict.Dict(Key, JsonString),
  )
}

pub fn builder_from(session: Session) {
  SessionBuilder(
    id: session.id,
    expiry: option.Some(
      ExpireAt(birl.from_erlang_universal_datetime(session.expires_at)),
    ),
    data: session.data,
  )
}

pub fn builder() {
  SessionBuilder(id: generate_id(), expiry: option.None, data: dict.new())
}

pub fn with_id(session: SessionBuilder, id: SessionId) {
  SessionBuilder(..session, id: id)
}

pub fn with_id_string(session: SessionBuilder, id: String) {
  SessionBuilder(..session, id: SessionId(id))
}

pub fn with_expiry(session: SessionBuilder, expiry: Expiry) {
  SessionBuilder(..session, expiry: option.Some(expiry))
}

pub fn with_expires_at(session: SessionBuilder, expires_at: birl.Time) {
  SessionBuilder(..session, expiry: option.Some(ExpireAt(expires_at)))
}

pub fn with_data(session: SessionBuilder, data: dict.Dict(Key, String)) {
  SessionBuilder(..session, data: data)
}

pub fn with_entry(session, key, data: String) {
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
