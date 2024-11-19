import birl
import birl/duration
import gleam/bit_array
import gleam/dict
import gleam/dynamic
import gleam/erlang/os
import gleam/http/response
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleeunit/should
import pog
import wisp
import wisp_kv_sessions
import wisp_kv_sessions/actor_adapter
import wisp_kv_sessions/postgres_adapter
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config

pub fn new_db() {
  let db_host = os.get_env("DB_HOST") |> result.unwrap("127.0.0.1")
  let db_password =
    os.get_env("DB_PASSWORD") |> result.unwrap("mySuperSecretPassword!")
  let db_user = os.get_env("DB_USER") |> result.unwrap("postgres")
  let assert Ok(db_port) =
    os.get_env("DB_PORT") |> result.unwrap("5432") |> int.parse
  let db_name = os.get_env("DB_NAME") |> result.unwrap("postgres")

  let db =
    pog.default_config()
    |> pog.host(db_host)
    |> pog.database(db_name)
    |> pog.port(db_port)
    |> pog.user(db_user)
    |> pog.password(option.Some(db_password))
    |> pog.pool_size(1)
    |> pog.connect()

  let assert Ok(_) = postgres_adapter.migrate_down(db)
  let assert Ok(_) = postgres_adapter.migrate_up(db)
  db
}

pub type TestObj {
  TestObj(test_field: String)
}

pub fn test_obj_to_json(obj: TestObj) {
  json.object([#("test_field", json.string(obj.test_field))])
  |> json.to_string
}

pub fn test_obj_from_json(json) {
  dynamic.decode1(TestObj, dynamic.field("test_field", of: dynamic.string))(
    json,
  )
}

pub fn test_session_key(current_session: wisp_kv_sessions.CurrentSession) {
  current_session
  |> wisp_kv_sessions.key("test_key")
  |> wisp_kv_sessions.with_codec(
    decoder: test_obj_from_json,
    encoder: test_obj_to_json,
  )
}

pub fn session_with_test_obj(expires_at) {
  let test_obj = TestObj(test_field: "test")
  let session =
    session.builder()
    |> session.with_id_string("TEST_SESSION_ID")
    |> session.with_expires_at(expires_at)
    |> session.with_data(
      dict.from_list([#("test_key", test_obj_to_json(test_obj))]),
    )
    |> session.build

  #(session, test_obj)
}

fn unwrap_cookie_value(value: String, req) {
  wisp.verify_signed_message(req, value) |> result.try(bit_array.to_string)
}

pub fn get_session_cookie_from_response(res, req) {
  res
  |> response.get_cookies
  |> list.key_find("SESSION_COOKIE")
  |> should.be_ok
  |> unwrap_cookie_value(req)
}

pub fn test_session_config() {
  let expiration = birl.now() |> birl.add(duration.days(3))
  use store <- result.map(actor_adapter.new())
  let session_config =
    session_config.Config(
      default_expiry: session.ExpireAt(expiration),
      cookie_name: "SESSION_COOKIE",
      store:,
      cache: option.None,
    )

  #(session_config, store, expiration)
}

pub fn test_session_config_with_cache() {
  let expiration = birl.now() |> birl.add(duration.days(3))
  use main_store <- result.try(actor_adapter.new())
  use cache_store <- result.map(actor_adapter.new())

  let session_config =
    session_config.Config(
      default_expiry: session.ExpireAt(expiration),
      cookie_name: "SESSION_COOKIE",
      store: main_store,
      cache: option.Some(cache_store),
    )

  #(session_config, main_store, cache_store, expiration)
}
