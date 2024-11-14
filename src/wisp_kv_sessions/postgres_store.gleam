import gleam/io
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import pog
import wisp
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config

pub fn new(db: pog.Connection) {
  session_config.SessionStore(
    get_session: get_session(db),
    save_session: save_session(db),
    delete_session: delete_session(db),
  )
}

pub fn migrate_up(db: pog.Connection) {
  let sql =
    "
    CREATE TABLE IF NOT EXISTS wisp_kv_sessions (
      session_id VARCHAR PRIMARY KEY,
      expires_at TIMESTAMP NOT NULL,
      data JSON
    );
    "

  pog.query(sql)
  |> pog.execute(db)
}

pub fn migrate_down(db: pog.Connection) {
  let sql = "DROP TABLE IF EXISTS wisp_kv_sessions"
  pog.query(sql)
  |> pog.execute(db)
}

fn get_session(db: pog.Connection) {
  fn(session_id: session.SessionId) {
    let sql =
      "
      SELECT session_id, expires_at, data from wisp_kv_sessions
      WHERE session_id = $1;
      "

    use returned <- result.try(
      pog.query(sql)
      |> pog.parameter(pog.text(session.id_to_string(session_id)))
      |> pog.returning(decode_session_row)
      |> pog.execute(db)
      |> result.map_error(fn(err) {
        io.debug(err)
        wisp.log_error(
          "Could not get session" <> session.id_to_string(session_id),
        )
        session.DbErrorGetError(
          "Could not get session with id"
          <> session.id_to_string(session_id)
          <> " from database",
        )
      }),
    )

    returned.rows
    |> list.first
    |> option.from_result
    |> option.map(fn(row) {
      use data <- result.map(
        decode_data_from_string(row.2)
        |> result.map_error(fn(err) {
          io.debug(err)
          session.DeserializeError("Could not deserialize data")
        }),
      )

      session.Session(
        id: session.id_from_string(row.0),
        expires_at: row.1,
        data: data,
      )
    })
    |> fn(r) {
      case r {
        option.Some(Ok(v)) -> Ok(option.Some(v))
        option.Some(Error(e)) -> Error(e)
        option.None -> Ok(option.None)
      }
    }
  }
}

fn save_session(db: pog.Connection) {
  fn(new_session: session.Session) {
    let sql =
      "
        INSERT INTO wisp_kv_sessions (session_id, expires_at, data)
        VALUES ($1, $2, $3)
        ON CONFLICT (session_id)
        DO UPDATE SET
            session_id = EXCLUDED.session_id,
            data = EXCLUDED.data,
            expires_at = EXCLUDED.expires_at;
      "

    // io.debug(birl.to_erlang_datetime(new_session.expires_at))
    let #(#(year, month, day), #(hour, minute, seconds)) =
      new_session.expires_at
    let insert =
      pog.query(sql)
      |> pog.parameter(pog.text(session.id_to_string(new_session.id)))
      |> pog.parameter(
        pog.timestamp(pog.Timestamp(
          pog.Date(year, month, day),
          pog.Time(hour, minute, seconds, 0),
        )),
      )
      |> pog.parameter(pog.text(json.to_string(encode_data(new_session.data))))
      |> pog.execute(db)

    // let insert =
    //   pog.execute(
    //     sql,
    //     db,
    //     [
    //       pog.text(session.id_to_string(new_session.id)),
    //       pog.timestamp(new_session.expires_at),
    //       pog.text(json.to_string(internal.encode_data(new_session.data))),
    //     ],
    //     fn(_) { Ok(Nil) },
    //   )

    case insert {
      Ok(_) -> Ok(new_session)
      Error(err) -> {
        io.debug(err)
        wisp.log_error("Could not insert new session")
        Error(session.DbErrorInsertError("Could not insert new session"))
      }
    }
  }
}

fn delete_session(db: pog.Connection) {
  fn(session_id: session.SessionId) {
    let sql =
      "
    DELETE FROM wisp_kv_sessions
    WHERE session_id = $1
    "

    case
      pog.query(sql)
      |> pog.parameter(pog.text(session.id_to_string(session_id)))
      |> pog.execute(db)
    {
      Ok(_) -> Ok(Nil)
      Error(err) -> {
        io.debug(err)
        wisp.log_error(
          "Could not delete session with id" <> session.id_to_string(session_id),
        )
        Error(session.DbErrorDeleteError(
          "Could not delete session with id" <> session.id_to_string(session_id),
        ))
      }
    }
  }
}

// Internal
//---------------

import gleam/dict
import gleam/dynamic

// Encode/Decode rows
//--------------------

@internal
pub fn decode_session_row(data: dynamic.Dynamic) {
  data
  |> dynamic.from
  |> dynamic.tuple3(
    dynamic.string,
    dynamic.tuple2(
      dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int),
      dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int),
    ),
    dynamic.string,
  )
}

@internal
pub fn encode_data(data: dict.Dict(String, String)) {
  data
  |> dict.fold([], fn(acc, key, val) {
    acc |> list.append([#(key, json.string(val))])
  })
  |> json.object
}

fn decode_data(data: dynamic.Dynamic) {
  dynamic.from(data) |> dynamic.dict(dynamic.string, dynamic.string)
}

@internal
pub fn decode_data_from_string(str: String) {
  json.decode(from: str, using: decode_data)
}
