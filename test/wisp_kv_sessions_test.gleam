import birl
import gleam/dict
import gleam/http/request
import gleam/http/response
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should
import test_helpers
import wisp
import wisp/testing
import wisp_kv_sessions
import wisp_kv_sessions/internal/utils
import wisp_kv_sessions/session

pub fn main() {
  gleeunit.main()
}

pub fn return_an_error_if_no_session_cookie_exist_test() {
  use #(session_config, _, _) <- result.map(test_helpers.test_session_config())
  let req = testing.get("/", [])

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> test_helpers.test_session_key()
  |> wisp_kv_sessions.set(test_helpers.TestObj(test_field: "test"))
  |> should.be_error
  |> should.equal(session.NoSessionError)
}

pub fn set_a_value_in_the_session_test() {
  use #(session_config, store, expires_at) <- result.map(
    test_helpers.test_session_config(),
  )
  use _ <- result.map(store.save_session(
    session.builder()
    |> session.with_id_string("TEST_SESSION_ID")
    |> session.with_expires_at(expires_at)
    |> session.build,
  ))
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let test_obj = test_helpers.TestObj(test_field: "test")

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> test_helpers.test_session_key()
  |> wisp_kv_sessions.set(test_obj)
  |> should.be_ok
  |> should.equal(test_obj)
}

pub fn get_a_value_from_the_session_test() {
  use #(session_config, store, expires_at) <- result.map(
    test_helpers.test_session_config(),
  )
  use _ <- result.map(store.save_session(
    session.builder()
    |> session.with_id_string("TEST_SESSION_ID")
    |> session.with_expires_at(expires_at)
    |> session.build,
  ))
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let test_obj = test_helpers.TestObj(test_field: "test")
  let _ =
    wisp_kv_sessions.CurrentSession(req, session_config)
    |> test_helpers.test_session_key()
    |> wisp_kv_sessions.set(test_obj)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> test_helpers.test_session_key()
  |> wisp_kv_sessions.get()
  |> should.be_ok
  |> should.be_some
  |> should.equal(test_obj)
}

pub fn delete_a_key_from_session_test() {
  use #(session_config, store, expires_at) <- result.map(
    test_helpers.test_session_config(),
  )
  let session_id = session.id_from_string("TEST_SESSION_ID")
  use _ <- result.map(
    store.save_session(session.Session(
      id: session_id,
      expires_at: birl.to_erlang_universal_datetime(expires_at),
      data: dict.from_list([
        #(
          "test_key",
          test_helpers.test_obj_to_json(test_helpers.TestObj("test")),
        ),
      ]),
    )),
  )
  use _ <- result.map(store.save_session(
    session.builder()
    |> session.with_id_string("TEST_SESSION_ID")
    |> session.with_expires_at(expires_at)
    |> session.with_data(
      dict.from_list([
        #(
          "test_key",
          test_helpers.test_obj_to_json(test_helpers.TestObj("test")),
        ),
      ]),
    )
    |> session.build,
  ))

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let _ =
    wisp_kv_sessions.CurrentSession(req, session_config)
    |> wisp_kv_sessions.delete("test_key")

  session_config.store.get_session(session_id)
  |> should.be_ok
  |> should.be_some
  |> fn(session: session.Session) { dict.get(session.data, "test_key") }
  |> should.be_error
}

pub fn delete_a_session_test() {
  use #(session_config, store, expires_at) <- result.map(
    test_helpers.test_session_config(),
  )
  use _ <- result.map(
    store.save_session(session.Session(
      id: session.id_from_string("TEST_SESSION_ID"),
      expires_at: birl.to_erlang_universal_datetime(expires_at),
      data: dict.from_list([
        #(
          "test_key",
          test_helpers.test_obj_to_json(test_helpers.TestObj("test")),
        ),
      ]),
    )),
  )
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> wisp_kv_sessions.delete_session()
  |> should.be_ok
  |> should.equal(Nil)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> wisp_kv_sessions.get_session()
  |> should.be_ok
}

pub fn creating_a_session_test() {
  use #(session_config, _store, expires_at) <- result.map(
    test_helpers.test_session_config(),
  )

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let session =
    wisp_kv_sessions.CurrentSession(req, session_config)
    |> wisp_kv_sessions.get_session()

  session
  |> should.be_ok
  |> fn(s: session.Session) { s.expires_at }
  |> should.equal(birl.to_erlang_universal_datetime(expires_at))
}

pub fn replace_session_test() {
  use #(session_config, actor_adapter, expires_at) <- result.map(
    test_helpers.test_session_config(),
  )
  let old_sesssion_id = session.id_from_string("TEST_SESSION_ID")
  use _ <- result.map(
    actor_adapter.save_session(session.Session(
      id: old_sesssion_id,
      expires_at: birl.to_erlang_universal_datetime(expires_at),
      data: dict.from_list([
        #(
          "test_key",
          test_helpers.test_obj_to_json(test_helpers.TestObj("test")),
        ),
      ]),
    )),
  )
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  let new_session = session.builder() |> session.build

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> wisp_kv_sessions.replace_session(wisp.ok(), new_session)
  |> should.be_ok
  |> test_helpers.get_session_cookie_from_response(req)
  |> should.be_ok
  |> should.equal(session.id_to_string(new_session.id))

  actor_adapter.get_session(new_session.id)
  |> should.be_ok
  |> should.be_some

  actor_adapter.get_session(old_sesssion_id)
  |> should.be_ok
  |> should.be_none
}

pub fn dont_get_expired_session_test() {
  use #(session_config, actor_adapter, _) <- result.map(
    test_helpers.test_session_config(),
  )

  let assert Ok(past_date) = birl.parse("1905-12-22 16:38:23-3")

  let expired_session =
    session.builder()
    |> session.with_expires_at(past_date)
    |> session.build

  let assert Ok(_) = actor_adapter.save_session(expired_session)

  let req =
    testing.get("/", [])
    |> testing.set_cookie(
      "SESSION_COOKIE",
      session.id_to_string(expired_session.id),
      wisp.Signed,
    )

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> wisp_kv_sessions.get_session()
  |> should.be_error
}

pub fn inject_a_cookie_in_a_request_test() {
  use #(session_config, _, _) <- result.map(test_helpers.test_session_config())

  let req =
    testing.get("/", [])
    |> utils.inject_session_cookie(
      session_config.cookie_name,
      _,
      session.id_from_string("session_id"),
      wisp.Signed,
    )

  wisp.get_cookie(req, "SESSION_COOKIE", wisp.Signed)
  |> should.be_ok
}

pub fn middleware_should_create_the_session_in_the_db_test() {
  use #(session_config, actor_adapter, _) <- result.map(
    test_helpers.test_session_config(),
  )

  let req = testing.get("/", [])
  let id =
    wisp_kv_sessions.middleware(req, session_config, fn(req) {
      wisp.get_cookie(req, "SESSION_COOKIE", wisp.Signed)
      |> should.be_ok

      wisp.ok()
    })
    |> test_helpers.get_session_cookie_from_response(req)
    |> should.be_ok

  actor_adapter.get_session(session.id_from_string(id))
  |> should.be_ok
  |> should.be_some
}

pub fn middleware_create_a_session_cookie_if_none_exist_test() {
  use #(session_config, _, _) <- result.map(test_helpers.test_session_config())

  wisp_kv_sessions.middleware(testing.get("/", []), session_config, fn(req) {
    wisp.get_cookie(req, "SESSION_COOKIE", wisp.Signed)
    |> should.be_ok

    wisp.ok()
  })
  |> response.get_cookies
  |> list.key_find("SESSION_COOKIE")
  |> should.be_ok
}

pub fn middleware_dont_set_cookie_if_its_set_in_handler_test() {
  use #(session_config, _, _) <- result.map(test_helpers.test_session_config())
  let session_id = session.generate_id()
  let req = testing.get("/", [])

  wisp_kv_sessions.middleware(req, session_config, fn(req) {
    wisp.ok()
    |> utils.set_session_cookie(
      session_config.cookie_name,
      _,
      req,
      session.builder()
        |> session.with_id(session_id)
        |> session.build(),
    )
  })
  |> test_helpers.get_session_cookie_from_response(req)
  |> should.be_ok
  |> should.equal(session.id_to_string(session_id))
}

pub fn should_override_exisiting_old_cookie_when_injecting_test() {
  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "NOT_VALID", wisp.PlainText)
    |> utils.inject_session_cookie(
      "SESSION_COOKIE",
      _,
      session.id_from_string("VALID_ID"),
      wisp.Signed,
    )

  wisp.get_cookie(req, "SESSION_COOKIE", wisp.Signed)
  |> should.be_ok
}

pub fn remove_cookie_from_request_test() {
  let req =
    testing.get("", [])
    |> request.set_cookie("FIRST_COOKIE", "first")
    |> request.set_cookie("SECOND_COOKIE", "second")
    |> request.set_cookie("THIRD_COOKIE", "third")

  req
  |> request.get_header("cookie")
  |> should.be_ok
  |> should.equal(
    "FIRST_COOKIE=first; SECOND_COOKIE=second; THIRD_COOKIE=third",
  )

  let modified_req =
    req
    |> utils.remove_cookie("SECOND_COOKIE")

  modified_req
  |> request.get_header("cookie")
  |> should.be_ok
  |> should.equal("FIRST_COOKIE=first; THIRD_COOKIE=third")
}
// Helpers
