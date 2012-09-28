package controllers

import java.util.UUID
import play.api.mvc._
import play.api.templates.{Html, HtmlFormat}

case class AuthToken(value: String) {
  def hiddenField: Html = {
    Html("""<input type="hidden" name="%s" value="%s" />""".format(HtmlFormat.escape(AuthenticationToken.AUTH_TOKEN), HtmlFormat.escape(value)))
  }
}

/**
 * provides mechanisms to protect actions against CRSF attacks
 */
trait AuthenticationToken {
  this: Controller =>

  /**
   * Get (or generate) an authentication token for the current session
   * @param result Continuation computing the result using the token
   */
  def authenticationToken(result: AuthToken => PlainResult)(implicit request: RequestHeader): PlainResult = {
    val token = session.get(AuthenticationToken.AUTH_TOKEN).getOrElse(UUID.randomUUID().toString)
    result(AuthToken(token)).withSession(AuthenticationToken.AUTH_TOKEN -> token)
  }

  /**
   * Check authenticity of action before performing an [[play.api.mvc.Action]]
   * @param action action to checked
   * @return result of action or a bad request if the authenticity cannot be proven
   */
  def checkAuthenticity[A](action: Action[A]): Action[A] = {
    Action(action.parser) { implicit request =>
      (for {
        token <- findAuthToken(request.body)
        authToken <- session.get(AuthenticationToken.AUTH_TOKEN)
        if AuthenticationToken.safeEquals(token, authToken)
      } yield {
        action(request)
      }).getOrElse(BadRequest("wrong security token"))
    }
  }

  /**
   * Try to find the Authentication Token in the request body
   */
  private def findAuthToken[A](content: A): Option[String] = content match {
    case AnyContentAsFormUrlEncoded(data)    => findAuthTokenInData(data)
    case AnyContentAsMultipartFormData(data) => findAuthTokenInData(data.dataParts)
    case mfd: MultipartFormData[_]           => findAuthTokenInData(mfd.dataParts)
    case _ => None
  }

  private def findAuthTokenInData( data : Map[String, Seq[String]] ) = {
    for {
      entry <- data.get(AuthenticationToken.AUTH_TOKEN)
      token <- entry.headOption
    } yield token
  }

}

object AuthenticationToken {

  val AUTH_TOKEN = "auth_token"

  // This method intentionally runs in constant time if the two strings have the same length.
  // If it didn't, it would be vulnerable to a timing attack.
  // credits to James Roper: https://github.com/playframework/Play20/commit/f981e2c2045e29c5928fdd3eb3717dbe3c0e9f9f
  def safeEquals(a: String, b: String) = {
    if (a.length != b.length) {
      false
    } else {
      var equal = 0
      for (i <- Array.range(0, a.length)) {
        equal |= a(i) ^ b(i)
      }
      equal == 0
    }
  }

}
