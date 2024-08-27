import birl
import birl/duration
import gleam/bit_array
import gleam/crypto
import gleam/http/request
import gleam/result
import internal/session_error
import internal/session_id
import session
import wisp

/// Inject a cookie into a request
/// This will NOT be persisted between requests
pub fn inject_session_cookie(
  cookie_name cookie_name: String,
  request req: wisp.Request,
  value session_id: session_id.SessionId,
  security security: wisp.Security,
) -> wisp.Request {
  let value = session_id.to_string(session_id)
  let value = case security {
    wisp.PlainText -> bit_array.base64_encode(<<value:utf8>>, False)
    wisp.Signed -> wisp.sign_message(req, <<value:utf8>>, crypto.Sha512)
  }
  req
  |> request.set_cookie(cookie_name, value)
}

// Helpers
pub fn set_session_cookie(
  cookie_name: String,
  response: wisp.Response,
  req: wisp.Request,
  session: session.Session,
) {
  wisp.set_cookie(
    response,
    req,
    cookie_name,
    session_id.to_string(session.id),
    wisp.Signed,
    seconds_from_now(session.expires_at),
  )
}

fn seconds_from_now(time: birl.Time) {
  time
  |> birl.difference(birl.now())
  |> duration.blur_to(duration.Second)
}

pub fn get_session_id(cookie_name: String, req: wisp.Request) {
  wisp.get_cookie(req, cookie_name, wisp.Signed)
  |> result.replace_error(session_error.NoSessionCookieError)
  |> result.map(session_id.SessionId)
}
