import wisp

// Session id
pub type SessionId {
  SessionId(String)
}

/// Create a new session id
pub fn generate() {
  SessionId(wisp.random_string(200))
}

/// Unwrap session id to a string
pub fn to_string(id: SessionId) {
  case id {
    SessionId(id_string) -> id_string
  }
}
