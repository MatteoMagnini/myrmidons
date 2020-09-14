package model

import akka.actor.{Actor, ActorRef}
import akka.actor.Props
import akka.event.Logging
import utility.Geometry.Vector2D
import utility.Messages.{StorageFood, TakeFood}

case class AnthillInfo(override val position: Vector2D, foodStorage: Int, maxStorageSpace: Int)
  extends SimpleObstacle (position, 50, 50) {
  def storage (q: Int): AnthillInfo = {
    AnthillInfo(position, foodStorage + q, maxStorageSpace)
  }
}

case class Anthill(info: AnthillInfo) extends Actor{

  val log = Logging(context.system, this)

  override def receive: Receive = defaultBehaviour(info)

  // actually the food stored could be infinite
  def defaultBehaviour(info: AnthillInfo): Receive = {
    case StorageFood(q) =>
      val tempInfo = info storage q
      log debug s"Storage q food ---> TOTAL ${info.foodStorage}"
      context become defaultBehaviour(tempInfo)

    case TakeFood(q) =>
      val tempInfo = takeFoodLogic(q, info)
      log debug s"taken food from storage ---> TOTAL ${info.foodStorage}"
      context become defaultBehaviour(tempInfo)
  }

  def takeFoodLogic(q: Int, info: AnthillInfo): AnthillInfo = q >= info.foodStorage match {
    case true =>
      sender ! TakeFood(info foodStorage)
      AnthillInfo(info position, 0, info maxStorageSpace)

    case _ =>
      sender ! TakeFood(q)
      info storage 0-q  //#workaround ;
  }
}

object Anthill {
  def apply(info: AnthillInfo): Props =
    Props(classOf[Anthill], info)
}
