import birl
import birl/duration
import gleam/bit_array
import gleam/dynamic
import gleam/http/response
import gleam/json
import gleam/list
import gleam/result
import gleeunit/should
import wisp
import wisp_kv_sessions/actor_store
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config

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
  use actor_store <- result.map(actor_store.try_create_session_store())
  let session_config =
    session_config.Config(
      default_expiry: session.ExpireAt(expiration),
      cookie_name: "SESSION_COOKIE",
      store: actor_store,
    )

  #(session_config, actor_store, expiration)
}
