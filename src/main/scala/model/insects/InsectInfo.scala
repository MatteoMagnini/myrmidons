package model.insects

import utility.Geometry._

object ConstantInsectInfo {

  def MAX_ENERGY = 100
  def MAX_FOOD = 3
  def STARTING_ENERGY = 100
  def STARTING_TIME = 0
  def STARTING_FOOD_AMOUNT = 0
  def STARTING_POSITION: Vector = ZeroVector2D()
}

import ConstantInsectInfo._

/**
 * The information in common with all kind of insects.
 */
trait InsectInfo {

  def id: Int
  def position: Vector
  def inertia: Vector
  def energy: Double
  def time: Int

  def updatePosition(newPosition: Vector): InsectInfo
  def updateInertia(newInertia: Vector): InsectInfo
  def updateEnergy(amount: Double): InsectInfo
  def incTime(): InsectInfo
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

  override def updateEnergy( delta: Double): InsectInfo =
    if (energy + delta > MAX_ENERGY)
      this.copy(energy = MAX_ENERGY)
    else
      this.copy(energy = energy + delta)

  override def incTime(): InsectInfo =
    this.copy(time = time + 1)

  def clearSensors(): ForagingAntInfo =
    this.copy(proximitySensor = ProximitySensor(), pheromoneSensor = PheromoneSensor())

  def addPheromones(pheromones: Iterable[Entity]): ForagingAntInfo =
    this.copy(pheromoneSensor = PheromoneSensor(pheromones))

  def incFood(amount: Int): ForagingAntInfo =
    if (foodAmount + amount > MAX_FOOD)
      this.copy(foodAmount = MAX_FOOD)
    else
      this.copy(foodAmount = foodAmount+amount)

  def freeFood(): ForagingAntInfo =
    this.copy(foodAmount = STARTING_FOOD_AMOUNT)

}

object ForagingAntInfo {
  def apply(id: Int = 0, position: Vector = STARTING_POSITION, energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): ForagingAntInfo =
    new ForagingAntInfo(id, ProximitySensor(), PheromoneSensor(), position, ZeroVector2D(), energy, time, STARTING_FOOD_AMOUNT)
}


