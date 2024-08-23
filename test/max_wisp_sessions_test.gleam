import birl
import birl/duration
import gleam/bit_array
import gleam/dict
import gleam/dynamic
import gleam/http/response
import gleam/json
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should
import internal/session_id
import max_wisp_sessions as sessions
import memory_store
import wisp
import wisp/testing

pub fn main() {
  gleeunit.main()
}

pub fn return_an_error_if_no_session_cookie_exist_test() {
  use #(session_config, _, _) <- result.map(test_session_config())
  let req = testing.get("/", [])

  sessions.set(
    session_config,
    req,
    "test_key",
    TestObj(test_field: "test"),
    test_obj_to_json,
  )
  |> should.be_error
  |> should.equal(sessions.NoSessionCookieError)
}

pub fn set_a_value_in_the_session_test() {
  use #(session_config, memory_store, expires_at) <- result.map(
    test_session_config(),
  )
  use _ <- result.map(
    memory_store.save_session(sessions.Session(
      id: session_id.SessionId("TEST_SESSION_ID"),
      expires_at: expires_at,
      data: dict.new(),
    )),
  )
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let test_obj = TestObj(test_field: "test")
  sessions.set(session_config, req, "test_key", test_obj, test_obj_to_json)
  |> should.be_ok
  |> should.equal(test_obj)
}

pub fn get_a_value_from_the_session_test() {
  use #(session_config, memory_store, expires_at) <- result.map(
    test_session_config(),
  )
  use _ <- result.map(
    memory_store.save_session(sessions.Session(
      id: session_id.SessionId("TEST_SESSION_ID"),
      expires_at: expires_at,
      data: dict.new(),
    )),
  )
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let test_obj = TestObj(test_field: "test")
  let _ =
    sessions.set(session_config, req, "test_key", test_obj, test_obj_to_json)

  sessions.get(session_config, req, "test_key", test_obj_from_json)
  |> should.be_ok
  |> should.be_some
  |> should.equal(test_obj)
}

pub fn delete_a_key_from_session_test() {
  use #(session_config, memory_store, expires_at) <- result.map(
    test_session_config(),
  )
  let session_id = session_id.SessionId("TEST_SESSION_ID")
  use _ <- result.map(
    memory_store.save_session(sessions.Session(
      id: session_id,
      expires_at: expires_at,
      data: dict.from_list([#("test_key", test_obj_to_json(TestObj("test")))]),
    )),
  )

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let _ = sessions.delete(session_config, req, "test_key")

  session_config.store.get_session(session_id)
  |> should.be_ok
  |> should.be_some
  |> fn(session: sessions.Session) { dict.get(session.data, "test_key") }
  |> should.be_error
}

pub fn delete_a_session_test() {
  use #(session_config, memory_store, expires_at) <- result.map(
    test_session_config(),
  )
  use _ <- result.map(
    memory_store.save_session(sessions.Session(
      id: session_id.SessionId("TEST_SESSION_ID"),
      expires_at: expires_at,
      data: dict.from_list([#("test_key", test_obj_to_json(TestObj("test")))]),
    )),
  )
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  sessions.delete_session(session_config, req)
  |> should.be_ok
  |> should.equal(Nil)

  sessions.get_session(session_config, req)
  |> should.be_ok
}

pub fn creating_a_session_test() {
  use memory_store <- result.map(memory_store.try_create_session_store())
  let expires_at = birl.now() |> birl.add(duration.days(3))
  let session_config =
    sessions.SessionConfig(
      default_expiry: sessions.ExpireAt(expires_at),
      cookie_name: "SESSION_COOKIE",
      store: memory_store,
    )
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let session = sessions.get_session(session_config, req)

  session
  |> should.be_ok
  |> sessions.session_expires_at
  |> should.equal(expires_at)
}

pub fn replace_session_test() {
  use #(session_config, memory_store, expires_at) <- result.map(
    test_session_config(),
  )
  let old_sesssion_id = session_id.SessionId("TEST_SESSION_ID")
  use _ <- result.map(
    memory_store.save_session(sessions.Session(
      id: old_sesssion_id,
      expires_at: expires_at,
      data: dict.from_list([#("test_key", test_obj_to_json(TestObj("test")))]),
    )),
  )
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let new_session = sessions.session_new(sessions.ExpireIn(100_000))

  sessions.replace_session(session_config, wisp.ok(), req, new_session)
  |> should.be_ok
  |> get_session_cookie_from_response(req)
  |> should.be_ok
  |> should.equal(session_id.to_string(new_session.id))

  memory_store.get_session(new_session.id)
  |> should.be_ok
  |> should.be_some

  memory_store.get_session(old_sesssion_id)
  |> should.be_ok
  |> should.be_none
}

pub fn inject_a_cookie_in_a_request_test() {
  use #(session_config, _, _) <- result.map(test_session_config())

  let req =
    testing.get("/", [])
    |> sessions.inject_session_cookie(
      session_config,
      _,
      session_id.SessionId("session_id"),
      wisp.Signed,
    )

  wisp.get_cookie(req, "SESSION_COOKIE", wisp.Signed)
  |> should.be_ok
}

pub fn middleware_create_a_session_cookie_if_none_exist_test() {
  use #(session_config, _, _) <- result.map(test_session_config())

  testing.get("/", [])
  |> sessions.middleware(
    session_config,
    _,
    fn(req) {
      wisp.get_cookie(req, "SESSION_COOKIE", wisp.Signed)
      |> should.be_ok

      wisp.ok()
    },
  )
  |> response.get_cookies
  |> list.key_find("SESSION_COOKIE")
  |> should.be_ok
}

pub fn middleware_dont_set_cookie_if_its_set_in_handler_test() {
  use #(session_config, _, _) <- result.map(test_session_config())
  let session_id = session_id.generate()
  let req = testing.get("/", [])

  req
  |> sessions.middleware(
    session_config,
    _,
    fn(req) {
      wisp.ok()
      |> sessions.set_session_cookie(
        session_config,
        _,
        req,
        sessions.session_new_with_id(session_id, sessions.ExpireIn(1_000_000)),
      )
    },
  )
  |> get_session_cookie_from_response(req)
  |> should.be_ok
  |> should.equal(session_id.to_string(session_id))
}

// Helpers
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

fn unwrap_cookie_value(value: String, req) {
  wisp.verify_signed_message(req, value) |> result.try(bit_array.to_string)
}

fn get_session_cookie_from_response(res, req) {
  res
  |> response.get_cookies
  |> list.key_find("SESSION_COOKIE")
  |> should.be_ok
  |> unwrap_cookie_value(req)
}

fn test_session_config() {
  let expiration = birl.now() |> birl.add(duration.days(3))
  use memory_store <- result.map(memory_store.try_create_session_store())
  let session_config =
    sessions.SessionConfig(
      default_expiry: sessions.ExpireAt(expiration),
      cookie_name: "SESSION_COOKIE",
      store: memory_store,
    )

  #(session_config, memory_store, expiration)
}
