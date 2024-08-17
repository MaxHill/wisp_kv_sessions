import birl
import gleam/bit_array
import gleam/crypto
import gleam/dict
import gleam/dynamic
import gleam/http/response
import gleam/io
import gleam/json
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should
import max_wisp_sessions as sessions
import memory_store
import wisp
import wisp/testing

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

pub type TestObj {
  TestObj(test_field: String)
}

fn test_obj_to_json(obj: TestObj) {
  json.object([#("test_field", json.string(obj.test_field))])
}

fn test_obj_from_json(json) {
  dynamic.decode1(TestObj, dynamic.field("test_field", of: dynamic.string))(
    json,
  )
}

pub fn it_returns_an_error_if_no_session_cookie_exist_test() {
  use memory_store <- result.map(memory_store.try_create_session_store())
  let req = testing.get("/", [])

  let assert Error(e) =
    sessions.set(
      memory_store,
      req,
      "test_key",
      TestObj(test_field: "test"),
      test_obj_to_json,
    )

  e
  |> should.equal(sessions.NoSessionCookieError)
}

pub fn it_can_set_a_value_in_the_session_test() {
  use memory_store <- result.map(memory_store.try_create_session_store())
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let test_obj = TestObj(test_field: "test")
  sessions.set(memory_store, req, "test_key", test_obj, test_obj_to_json)
  |> should.be_ok
  |> should.equal(test_obj)
}

pub fn it_can_get_a_value_from_the_session_test() {
  use memory_store <- result.map(memory_store.try_create_session_store())
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let test_obj = TestObj(test_field: "test")
  let _ =
    sessions.set(memory_store, req, "test_key", test_obj, test_obj_to_json)

  sessions.get(memory_store, req, "test_key", test_obj_from_json)
  |> should.be_ok
  |> should.be_some
  |> should.equal(test_obj)
}

pub fn it_can_delete_a_specific_session_test() {
  use memory_store <- result.map(memory_store.try_create_session_store())
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let test_obj = TestObj(test_field: "test")
  let _ =
    sessions.set(memory_store, req, "test_key", test_obj, test_obj_to_json)

  sessions.delete(memory_store, req)
  |> should.be_ok
  |> should.equal(Nil)

  sessions.get(memory_store, req, "test_key", test_obj_from_json)
  |> should.be_ok
  |> should.be_none
}

pub fn it_can_inject_a_cookie_in_a_request_test() {
  let req =
    testing.get("/", [])
    |> sessions.inject_session_cookie("session_id", wisp.Signed)

  wisp.get_cookie(req, "SESSION_COOKIE", wisp.Signed)
  |> should.be_ok
}

pub fn it_should_create_a_session_cokie_if_none_exist_test() {
  testing.get("/", [])
  |> sessions.middleware(fn(req) {
    wisp.get_cookie(req, "SESSION_COOKIE", wisp.Signed)
    |> should.be_ok

    wisp.ok()
  })
  |> response.get_cookies
  |> list.key_find("SESSION_COOKIE")
  |> should.be_ok
}

pub fn middleware_should_not_set_cookie_if_its_set_in_handler_test() {
  let req = testing.get("/", [])

  let unsign = fn(value) {
    wisp.verify_signed_message(req, value) |> result.try(bit_array.to_string)
  }

  req
  |> sessions.middleware(fn(req) {
    wisp.ok()
    |> sessions.set_session_cookie(req, "some_id", 60 * 60)
  })
  |> response.get_cookies
  |> list.key_find("SESSION_COOKIE")
  |> should.be_ok
  |> unsign
}
