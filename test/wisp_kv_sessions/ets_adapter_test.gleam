import gleeunit/should
import wisp_kv_sessions/ets_adapter
import wisp_kv_sessions/session

pub fn set_get_session_test() {
  let session =
    session.builder()
    |> session.with_entry("test", "hello")
    |> session.build

  let session_store = ets_adapter.new("test_table")

  session_store.save_session(session)
  |> should.be_ok()
  |> should.equal(session)

  session_store.get_session(session.id)
  |> should.be_ok()
  |> should.be_some()
  |> should.equal(session)
}

pub fn set_delete_session_test() {
  let session =
    session.builder()
    |> session.with_entry("test", "hello2")
    |> session.build

  let session_store = ets_adapter.new("test_table2")

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
