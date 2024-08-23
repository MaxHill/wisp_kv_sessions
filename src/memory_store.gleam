import gleam/dict
import gleam/erlang/process
import gleam/option
import gleam/otp/actor
import gleam/result
import internal/session_id
import max_wisp_sessions as sessions
import session

const timeout = 3000

type Message(element) {
  Shutdown
  GetSession(
    reply_with: process.Subject(Result(session.Session, Nil)),
    session_id: session.SessionId,
  )
  SetSession(
    reply_with: process.Subject(session.Session),
    session: session.Session,
  )
  DeleteSession(session_id: session.SessionId)
}

type Db =
  dict.Dict(session_id.SessionId, session.Session)

pub fn try_create_session_store() {
  use db <- result.map(
    actor.start(dict.new(), handle_message)
    |> result.replace_error(sessions.DbSetupError),
  )
  sessions.SessionStore(
    default_expiry: 60 * 60,
    get_session: get_session(db),
    save_session: save_session(db),
    delete_session: delete_session(db),
  )
}

fn get_session(db: process.Subject(Message(a))) {
  fn(session_id: session.SessionId) {
    Ok(
      process.call(db, fn(client) { GetSession(client, session_id) }, timeout)
      |> result.map(fn(session) { option.Some(session) })
      |> option.from_result
      |> option.flatten,
    )
  }
}

fn save_session(db: process.Subject(Message(a))) {
  fn(session: session.Session) {
    Ok(process.call(db, fn(client) { SetSession(client, session) }, timeout))
  }
}

fn delete_session(db: process.Subject(Message(a))) {
  fn(session_id: session.SessionId) {
    process.send(db, DeleteSession(session_id))
    Ok(Nil)
  }
}

fn handle_message(message: Message(e), db: Db) -> actor.Next(Message(e), Db) {
  case message {
    Shutdown -> actor.Stop(process.Normal)
    GetSession(client, session_id) -> {
      let session = dict.get(db, session_id)
      process.send(client, session)
      actor.continue(db)
    }
    SetSession(client, session) -> {
      process.send(client, session)
      actor.continue(dict.insert(db, session.id, session))
    }
    DeleteSession(session_id) -> {
      actor.continue(dict.delete(db, session_id))
    }
  }
}
