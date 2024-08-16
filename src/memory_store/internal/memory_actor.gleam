import birl
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/json.{type Json}
import gleam/otp/actor
import gleam/result
import max_wisp_sessions as sessions

pub type Message(element) {
  Shutdown
  Clear(session_id: sessions.SessionId)
  Set(session_id: sessions.SessionId, key: sessions.Key, data: Json)
  GetSession(
    reply_with: Subject(Result(sessions.Session, Nil)),
    session_id: sessions.SessionId,
  )
  GetField(
    reply_with: Subject(Result(Json, Nil)),
    session_id: sessions.SessionId,
    key: sessions.Key,
  )
}

type Db =
  dict.Dict(String, sessions.Session)

pub fn handle_message(message: Message(e), db: Db) -> actor.Next(Message(e), Db) {
  case message {
    Shutdown -> actor.Stop(process.Normal)
    Clear(session_id) -> {
      actor.continue(dict.drop(db, [session_id]))
    }
    Set(session_id, key, data) -> {
      let new_session =
        dict.get(db, session_id)
        |> result.unwrap(sessions.Session(
          id: session_id,
          expiry: birl.now(),
          data: dict.new(),
        ))
        |> fn(session) {
          sessions.Session(
            ..session,
            data: dict.insert(session.data, key, data),
          )
        }

      actor.continue(dict.insert(db, session_id, new_session))
    }
    GetSession(client, session_id) -> {
      let session = dict.get(db, session_id)
      process.send(client, session)
      actor.continue(db)
    }
    GetField(client, session_id, key) -> {
      let field =
        dict.get(db, session_id)
        |> result.try(fn(session) { dict.get(session.data, key) })

      process.send(client, field)
      actor.continue(db)
    }
  }
}
