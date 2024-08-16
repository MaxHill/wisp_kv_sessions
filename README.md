# max_wisp_sessions

[![Package Version](https://img.shields.io/hexpm/v/max_wisp_sessions)](https://hex.pm/packages/max_wisp_sessions)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/max_wisp_sessions/)

```sh
gleam add max_wisp_sessions@1
```
```gleam
import max_wisp_sessions

pub fn main() {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/max_wisp_sessions>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```


# Stub for creating a new SessionStore
```
pub type PostgresSessionStore {
  PostgresSessionStore(db: String)
}

pub fn create_postgres_session_store(config: PostgresSessionStore) {
  SessionStore(
    setup: setup(config),
    get_session: get_session(config),
    get: fn(session_id: SessionId, key: String) { get(config, session_id, key) },
    set: fn(session_id: SessionId, key: String, value: Json) {
      set(config, session_id, key, value)
    },
    remove: remove(config, _),
  )
}

fn setup(config: PostgresSessionStore) {
  fn() { todo }
}

fn get_session(config: PostgresSessionStore) {
  fn(session_id: SessionId) { todo }
}

fn get(config: PostgresSessionStore) {
  fn(session_id: SessionId, key: String) { todo }
}

fn set(config: PostgresSessionStore) {
  fn(session_id: SessionId, key: String, data: Json) { todo }
}

fn remove(config: PostgresSessionStore, session_id: SessionId) {
  fn(session_id: SessionId) { todo }
}
```
