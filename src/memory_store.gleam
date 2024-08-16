import birl
import gleam/dict
import gleam/erlang/process
import gleam/json.{type Json}
import gleam/option
import gleam/otp/actor
import gleam/result
import max_wisp_sessions as sessions
import memory_store/internal/memory_actor

const timeout = 3000

// Memory SessionStore Example
type MemorySessionStoreConfig {
  MemorySessionStoreConfig(db: dict.Dict(String, dict.Dict(String, Json)))
}

pub fn try_create_session_store() {
  let _config = MemorySessionStoreConfig(db: dict.new())
  use db <- result.map(
    actor.start(dict.new(), memory_actor.handle_message)
    |> result.replace_error(sessions.DbSetupError),
  )
  sessions.SessionStore(
    get_session: get_session(db),
    get: get(db),
    set: set(db),
    delete: delete(db),
  )
}

fn get_session(db: process.Subject(memory_actor.Message(a))) {
  fn(session_id: sessions.SessionId) {
    process.call(
      db,
      fn(client) { memory_actor.GetSession(client, session_id) },
      timeout,
    )
    |> result.unwrap(sessions.Session(
      id: session_id,
      expiry: birl.now(),
      data: dict.new(),
    ))
  }
}

fn get(db: process.Subject(memory_actor.Message(a))) {
  fn(session_id: sessions.SessionId, key: sessions.Key) {
    process.call(
      db,
      fn(client) { memory_actor.GetField(client, session_id, key) },
      timeout,
    )
    |> option.from_result
  }
}

fn set(db: process.Subject(memory_actor.Message(a))) {
  fn(session_id: sessions.SessionId, key: sessions.Key, data: Json) {
    process.send(db, memory_actor.Set(session_id, key, data))
    Ok(Nil)
  }
}

fn delete(db: process.Subject(memory_actor.Message(a))) {
  fn(session_id: sessions.SessionId) {
    process.send(db, memory_actor.Clear(session_id))
    Ok(Nil)
  }
}
