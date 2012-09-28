package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import models.Order

/**
 * A flow not protected against CRSF attacks
 */
object Application extends Controller {

  val orderForm = Form(
    mapping(
      "id" -> text
    )(Order.apply)(Order.unapply)
  )

  def index = Action {
    Ok(views.html.index(orderForm))
  }

  def deleteOrder = Action { implicit request =>
    orderForm.bindFromRequest().fold(
      errors => BadRequest(views.html.index(errors)),

      order => Ok(views.html.result(order.id))
    )
  }

}
