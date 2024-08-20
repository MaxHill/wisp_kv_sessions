import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/json.{type Json}
import gleam/otp/actor
import gleam/result
import max_wisp_sessions as sessions

pub type Message(element) {
  Shutdown
  Clear(session_id: sessions.SessionId)
  GetSession(
    reply_with: Subject(Result(sessions.Session, Nil)),
    session_id: sessions.SessionId,
  )
  CreateSession(
    reply_with: Subject(sessions.Session),
    session: sessions.Session,
  )
  SetField(session_id: sessions.SessionId, key: sessions.Key, data: Json)
  GetField(
    reply_with: Subject(Result(Json, Nil)),
    session_id: sessions.SessionId,
    key: sessions.Key,
  )
}

type Db =
  dict.Dict(sessions.SessionId, sessions.Session)

pub fn handle_message(message: Message(e), db: Db) -> actor.Next(Message(e), Db) {
  case message {
    Shutdown -> actor.Stop(process.Normal)
    Clear(session_id) -> {
      actor.continue(dict.drop(db, [session_id]))
    }
    GetSession(client, session_id) -> {
      let session = dict.get(db, session_id)
      process.send(client, session)
      actor.continue(db)
    }
    CreateSession(client, session) -> {
      process.send(client, session)
      actor.continue(dict.insert(db, session.id, session))
    }
    SetField(session_id, key, data) -> {
      let new_session =
        dict.get(db, session_id)
        |> result.unwrap(sessions.session_new_with_id(
          session_id,
          sessions.ExpireIn(60 * 60),
        ))
        |> fn(session) { sessions.session_set_key_value(session, key, data) }

      actor.continue(dict.insert(db, session_id, new_session))
    }
    GetField(client, session_id, key) -> {
      let field =
        dict.get(db, session_id)
        |> result.try(fn(session) { sessions.session_get(session, key) })

      process.send(client, field)
      actor.continue(db)
    }
  }
}
