import gleam/dict
import gleam/result
import gleeunit/should
import test_helpers
import wisp
import wisp/testing
import wisp_kv_sessions
import wisp_kv_sessions/session

pub fn get_value_from_cache_test() {
  use #(session_config, main_store, cache_store, expires_at) <- result.map(
    test_helpers.test_session_config_with_cache(),
  )
  let #(session, test_obj) = test_helpers.session_with_test_obj(expires_at)

  use _ <- result.map(cache_store.save_session(session))

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> test_helpers.test_session_key
  |> wisp_kv_sessions.get()
  |> should.be_ok
  |> should.be_some
  |> should.equal(test_obj)

  main_store.get_session(session.id)
  |> should.be_ok
  |> should.be_none
}

pub fn get_value_from_falls_back_to_main_test() {
  use #(session_config, main_store, cache_store, expires_at) <- result.map(
    test_helpers.test_session_config_with_cache(),
  )
  let #(session, test_obj) = test_helpers.session_with_test_obj(expires_at)
  use _ <- result.map(main_store.save_session(session))

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> test_helpers.test_session_key
  |> wisp_kv_sessions.get
  |> should.be_ok
  |> should.be_some
  |> should.equal(test_obj)

  cache_store.get_session(session.id)
  |> should.be_ok
  |> should.be_none
}

pub fn set_value_in_session_test() {
  use #(session_config, main_store, cache_store, expires_at) <- result.map(
    test_helpers.test_session_config_with_cache(),
  )
  let #(session, test_obj) = test_helpers.session_with_test_obj(expires_at)

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> test_helpers.test_session_key()
  |> wisp_kv_sessions.set(test_obj)
  |> should.be_ok

  cache_store.get_session(session.id)
  |> should.be_ok
  |> should.be_some
  |> should.equal(session)

  main_store.get_session(session.id)
  |> should.be_ok
  |> should.be_some
  |> should.equal(session)
}

pub fn delete_value_from_cache_test() {
  use #(session_config, main_store, cache_store, expires_at) <- result.try(
    test_helpers.test_session_config_with_cache(),
  )
  let #(session, _test_obj) = test_helpers.session_with_test_obj(expires_at)

  use _ <- result.map(cache_store.save_session(session))

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> wisp_kv_sessions.delete("test_key")
  |> should.be_ok

  cache_store.get_session(session.id)
  |> should.be_ok
  |> should.be_some
  |> fn(sess: session.Session) { dict.get(sess.data, "test_key") }
  |> should.be_error

  main_store.get_session(session.id)
  |> should.be_ok
  |> should.be_some
  |> fn(sess: session.Session) { dict.get(sess.data, "test_key") }
  |> should.be_error
}

pub fn get_session_test() {
  use #(session_config, _main_store, cache_store, expires_at) <- result.try(
    test_helpers.test_session_config_with_cache(),
  )
  let #(session, _test_obj) = test_helpers.session_with_test_obj(expires_at)

  use _ <- result.map(cache_store.save_session(session))

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> wisp_kv_sessions.get_session()
  |> should.be_ok()
  |> should.equal(session)
}

pub fn get_session_falls_back_to_main_test() {
  use #(session_config, main_store, _cache_store, expires_at) <- result.try(
    test_helpers.test_session_config_with_cache(),
  )
  let #(session, _test_obj) = test_helpers.session_with_test_obj(expires_at)

  use _ <- result.map(main_store.save_session(session))

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> wisp_kv_sessions.get_session()
  |> should.be_ok()
  |> should.equal(session)
}

pub fn caches_new_session_on_create_test() {
  use #(session_config, _main_store, cache_store, expires_at) <- result.map(
    test_helpers.test_session_config_with_cache(),
  )

  // This is the session that will be created
  let session =
    session.builder()
    |> session.with_id_string("TEST_SESSION_ID")
    |> session.with_expires_at(expires_at)
    |> session.build

  let req =
    testing.get("/", [])
    |> testing.set_cookie("SESSION_COOKIE", "TEST_SESSION_ID", wisp.Signed)

  wisp_kv_sessions.CurrentSession(req, session_config)
  |> wisp_kv_sessions.get_session()
  |> should.be_ok()
  |> should.equal(session)

  cache_store.get_session(session.id_from_string("TEST_SESSION_ID"))
  |> should.be_ok()
  |> should.be_some()
  |> should.equal(session)
}
