package model.anthill

import akka.actor.{Actor, ActorRef, Props}
import model.Drawable
import utility.Geometry.{OrientedVector2DWithNoise, Vector2D, ZeroVector2D}
import utility.Messages._

case class AnthillInfo(override val position: Vector2D,
                       radius: Double,
                       foodAmount: Double,
                       maxFoodAmount: Double) extends Drawable {

  def incFood(delta: Double): AnthillInfo =
    this.copy(foodAmount = if (foodAmount + delta > maxFoodAmount) maxFoodAmount else foodAmount + delta)

  def decFood(delta: Double): AnthillInfo =
    this.copy(foodAmount = if (foodAmount - delta < 0) 0 else foodAmount - delta)
}

object AnthillInfo {
  def apply(position: Vector2D, radius: Double = 3, foodAmount: Double = 0, maxFoodAmount: Double = 10000): AnthillInfo =
    new AnthillInfo(position, radius, foodAmount, maxFoodAmount)
}

case class Anthill(info: AnthillInfo, environment: ActorRef) extends Actor {

  override def receive: Receive = defaultBehaviour(info)

  def defaultBehaviour(data: AnthillInfo): Receive = {

    case StoreFood(delta) =>
      println("Anthill: " + data.foodAmount)
      context become defaultBehaviour(data.incFood(delta))

    case EatFood(delta) =>
      val newDelta = if (data.foodAmount > delta) delta else data.foodAmount
      val newData = data.decFood(newDelta)
      sender ! EatFood(newDelta)
      context become defaultBehaviour(newData)

    case AntTowardsAnthill(position, maxSpeed, noise, antIsIn) =>
      val dist = info.position - position
      if (!antIsIn && dist.|| <= data.radius) {
        sender ! UpdateAnthillCondition(true)
        environment.tell(Move(position, ZeroVector2D()), sender)
      } else {
        val rad = dist./\
        val delta = OrientedVector2DWithNoise(rad, maxSpeed, noise)
        environment.tell(Move(position, delta), sender)
      }

    case Clock(_) =>
      environment ! UpdateAnthill(data)

  }
}

object Anthill {
  def apply(info: AnthillInfo, environment: ActorRef): Props =
    Props(classOf[Anthill], info, environment)
}
