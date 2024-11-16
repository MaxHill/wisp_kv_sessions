import gleeunit/should
import test_helpers
import wisp_kv_sessions/postgres_adapter
import wisp_kv_sessions/session

pub fn set_get_session_test() {
  let db = test_helpers.new_db()
  let session =
    session.builder()
    |> session.with_entry("test", "hello")
    |> session.build

  let session_store = postgres_adapter.new(db)

  session_store.save_session(session)
  |> should.be_ok()
  |> should.equal(session)

  session_store.get_session(session.id)
  |> should.be_ok()
  |> should.be_some()
  |> should.equal(session)
}

pub fn set_delete_session_test() {
  let db = test_helpers.new_db()
  let session =
    session.builder()
    |> session.with_entry("test", "hello")
    |> session.build

  let session_store = postgres_adapter.new(db)

  session_store.save_session(session)
  |> should.be_ok()
  |> should.equal(session)

  session_store.delete_session(session.id)
  |> should.be_ok()
  |> should.equal(Nil)

  session_store.get_session(session.id)
  |> should.be_ok()
  |> should.be_none()
}

// Internal
//---------------
import gleam/dict
import gleam/dynamic
import gleam/json

pub fn encode_data_test() {
  let data =
    dict.new()
    |> dict.insert("key_one", "value1")
    |> dict.insert(
      "key_two",
      json.to_string(json.object([#("value2.1", json.string("value2.2"))])),
    )
    |> dict.insert("key_three", "value3")

  let json = postgres_adapter.encode_data(data)

  json
  |> json.to_string
  |> should.equal(
    "{\"key_one\":\"value1\",\"key_three\":\"value3\",\"key_two\":\"{\\\"value2.1\\\":\\\"value2.2\\\"}\"}",
  )

  json
  |> json.to_string
  |> json.decode(dynamic.dict(dynamic.string, dynamic.string))
  |> should.be_ok
  |> dict.get("key_one")
  |> should.be_ok
  |> should.equal("value1")

  json
  |> json.to_string
  |> json.decode(dynamic.dict(dynamic.string, dynamic.string))
  |> should.be_ok
  |> dict.get("key_two")
  |> should.be_ok
  |> should.equal("{\"value2.1\":\"value2.2\"}")

  json
  |> json.to_string
  |> postgres_adapter.decode_data_from_string()
  |> should.be_ok
  |> should.equal(data)
}
