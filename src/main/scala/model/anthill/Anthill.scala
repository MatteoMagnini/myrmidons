package model.anthill

import akka.actor.{Actor, ActorRef, Props}
import model.Drawable
import utility.Geometry.{OrientedVector2D, Vector2D}
import utility.Messages.{AntTowardsAnthill, Clock, Move, StoreFood, TakeFood, UpdateAnthill}

case class AnthillInfo(override val position: Vector2D,
                       foodAmount: Double,
                       maxFoodAmount: Double) extends Drawable{

  def incFood(delta: Double): AnthillInfo =
    this.copy(foodAmount = if (foodAmount + delta > maxFoodAmount) maxFoodAmount else foodAmount + delta)

  def decFood(delta: Double): AnthillInfo =
    this.copy(foodAmount = if (foodAmount - delta < 0) 0 else foodAmount - delta)
}

object AnthillInfo {
  def apply( position: Vector2D, foodAmount: Double = 0, maxFoodAmount: Double = 1000): AnthillInfo =
    new AnthillInfo(position, foodAmount, maxFoodAmount)
}

case class Anthill(info: AnthillInfo, environment: ActorRef) extends Actor {

  override def receive: Receive = defaultBehaviour(info)

  def defaultBehaviour(data: AnthillInfo): Receive = {

    case StoreFood(delta) =>
      context become defaultBehaviour(data.incFood(delta))

    case TakeFood(delta) =>
      val newData = data.decFood(delta)
      sender ! TakeFood(data.foodAmount - newData.foodAmount)
      context become defaultBehaviour(newData)

    case AntTowardsAnthill(position, maxSpeed) =>
      val rad = (info.position - position)./\
      environment.tell(Move(position, OrientedVector2D(rad, maxSpeed)), sender)

    case Clock(_) =>
      environment ! UpdateAnthill(data)

  }
}

object Anthill {
  def apply(info: AnthillInfo, environment: ActorRef): Props =
    Props(classOf[Anthill], info, environment)
}
