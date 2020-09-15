package model.insects

import akka.actor.ActorRef
import model.Drawable
import utility.Geometry._

object ConstantInsectInfo {

  def MAX_ENERGY = 100
  def MAX_FOOD = 3
  def STARTING_ENERGY = 100
  def STARTING_TIME = 0
  def STARTING_FOOD_AMOUNT = 0
  def STARTING_POSITION: Vector2D = ZeroVector2D()
}

import ConstantInsectInfo._

/**
 * The information in common with all kind of insects.
 */

trait InsectInfo extends Drawable {

  def id: Int
  def inertia: Vector2D
  def energy: Double
  def time: Int
  def anthill: ActorRef

  def updatePosition(newPosition: Vector2D): InsectInfo
  def updateInertia(newInertia: Vector2D): InsectInfo
  def updateEnergy(amount: Double): InsectInfo
  def incTime(): InsectInfo
}

case class ForagingAntInfo(override val anthill: ActorRef,
                           override val id: Int,
                           proximitySensor: Sensor,
                           pheromoneSensor: Sensor,
                           override val position: Vector2D,
                           override val inertia: Vector2D,
                           override val energy: Double,
                           override val time: Int,
                           foodAmount: Double) extends InsectInfo {

  override def updatePosition(newPosition: Vector2D): InsectInfo =
    this.copy(position = newPosition)

  override def updateInertia(newInertia: Vector2D): InsectInfo =
    this.copy(inertia = newInertia)

  override def updateEnergy(delta: Double): InsectInfo =
    this.copy(energy = if (energy + delta > MAX_ENERGY) MAX_ENERGY else energy + delta)

  override def incTime(): InsectInfo =
    this.copy(time = time + 1)

  def clearSensors(): ForagingAntInfo =
    this.copy(proximitySensor = ProximitySensor(), pheromoneSensor = PheromoneSensor())

  def addPheromones(pheromones: Iterable[Entity]): ForagingAntInfo =
    this.copy(pheromoneSensor = PheromoneSensor(pheromones))

  def incFood(amount: Double): ForagingAntInfo =
    this.copy(foodAmount = if (foodAmount + amount > MAX_FOOD) MAX_FOOD else foodAmount + amount)

  def freeFood(): ForagingAntInfo =
    this.copy(foodAmount = STARTING_FOOD_AMOUNT)

}

object ForagingAntInfo {
  def apply(anthill: ActorRef, id: Int = 0, position: Vector2D = STARTING_POSITION, energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): ForagingAntInfo =
    new ForagingAntInfo(anthill, id, ProximitySensor(), PheromoneSensor(), position, ZeroVector2D(), energy, time, STARTING_FOOD_AMOUNT)
}


