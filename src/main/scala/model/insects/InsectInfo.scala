package model.insects

import utility.Geometry._

object Constant {

  def MAX_ENERGY = 100
  def MAX_FOOD = 3
  def STARTING_ENERGY = 100
  def STARTING_TIME = 0
  def STARTING_FOOD_AMOUNT = 0
  def STARTING_POSITION: Vector = ZeroVector2D()
}

import Constant._

/**
 * The information in common with all kind of insects.
 */
trait InsectInfo {

  private val SEPARATOR = " "

  def id: Int
  def position: Vector
  def inertia: Vector
  def energy: Double
  def time: Int

  def updatePosition(newPosition: Vector): InsectInfo
  def updateInertia(newInertia: Vector): InsectInfo
  def updateEnergy(amount: Double): InsectInfo
  def incTime(): InsectInfo

  override def toString: String = position + SEPARATOR + energy + SEPARATOR + time + SEPARATOR + super.toString
}

case class ForagingAntInfo(override val id: Int,
                            proximitySensor: Sensor,
                           pheromoneSensor: Sensor,
                           override val position: Vector,
                           override val inertia: Vector,
                           override val energy: Double,
                           override val time: Int,
                           foodAmount: Int) extends InsectInfo {

  override def updatePosition(newPosition: Vector): InsectInfo =
    this.copy(position = newPosition)

  override def updateInertia(newInertia: Vector): InsectInfo =
    this.copy(inertia = newInertia)

  override def updateEnergy(amount: Double): InsectInfo =
    if (energy + amount > MAX_ENERGY)
      this.copy(energy = MAX_ENERGY)
    else
      this.copy(energy = energy + amount)

  override def incTime(): InsectInfo =
    this.copy(time = time + 1)

  def clearSensors(): InsectInfo =
    this.copy(proximitySensor = ProximitySensor(), pheromoneSensor = PheromoneSensor())

  def addPheromones(pheromones: Iterable[Entity]): InsectInfo =
    this.copy(pheromoneSensor = updateSensor(pheromones,proximitySensor))

  @scala.annotation.tailrec
  private def updateSensor( entities: Iterable[Entity], sensor: Sensor): Sensor =
    if (entities.isEmpty)
      sensor
    else
      updateSensor(entities.takeRight(entities.size - 1), sensor.addEntity(entities.take(1).last))

  def incFood(amount: Int): InsectInfo =
    if (foodAmount + amount > MAX_FOOD)
      this.copy(foodAmount = MAX_FOOD)
    else
      this.copy(foodAmount = foodAmount+amount)

  def freeFood(): InsectInfo =
    this.copy(foodAmount = STARTING_FOOD_AMOUNT)

}

object ForagingAntInfo {
  def apply(id: Int = 0, position: Vector = STARTING_POSITION, energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): ForagingAntInfo =
    new ForagingAntInfo(id, ProximitySensor(), PheromoneSensor(), position, ZeroVector2D(), energy, time, STARTING_FOOD_AMOUNT)
}


