package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import models.Order

/**
 * A flow protected against CRSF attacks
 * views.html.index_protected needs an [[controllers.AuthToken]]
 */
object ProtectedApplication extends Controller with AuthenticationToken {

  val orderForm = Form(
    mapping(
      "id" -> text
    )(Order.apply)(Order.unapply)
  )

  def index = Action { implicit request =>
    authenticationToken { implicit token =>
      Ok(views.html.index_protected(orderForm))
    }
  }

  /**
   * Delete an order
   * checks the authenticity token before deleting
   */
  def deleteOrder = checkAuthenticity {
    Action { implicit request =>
      orderForm.bindFromRequest().fold(
        errors => {
          authenticationToken { implicit token =>
            BadRequest(views.html.index_protected(errors))
          }
        },

        order => Ok(views.html.result(order.id))
      )
    }
  }

}
