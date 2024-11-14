import carpenter/table
import gleam/list
import gleam/option
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config

pub fn new(table_name) {
  let db = new_table(table_name)
  session_config.SessionStore(
    get_session: get_session(db),
    save_session: save_session(db),
    delete_session: delete_session(db),
  )
}

pub fn new_table(table_name) -> table.Set(String, session.Session) {
  // Set up and configure an ETS table
  let assert Ok(table) =
    table.build(table_name)
    |> table.privacy(table.Private)
    |> table.write_concurrency(table.AutoWriteConcurrency)
    |> table.read_concurrency(True)
    |> table.decentralized_counters(True)
    |> table.compression(False)
    |> table.set

  table
}

fn get_session(db) {
  fn(session_id: session.SessionId) {
    let res =
      db
      |> table.lookup(session.id_to_string(session_id))
      |> list.first

    case res {
      Ok(tup) -> {
        Ok(option.Some(tup.1))
      }
      Error(_) -> {
        Ok(option.None)
      }
    }
  }
}

fn save_session(db) {
  fn(new_session: session.Session) {
    db
    |> table.insert([#(session.id_to_string(new_session.id), new_session)])
    Ok(new_session)
  }
}

fn delete_session(db) {
  fn(session_id: session.SessionId) {
    db
    |> table.delete(session.id_to_string(session_id))
    Ok(Nil)
  }
}
