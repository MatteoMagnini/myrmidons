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

case class ForagingAntInfo(proximitySensor: Sensor,
                           pheromoneSensor: Sensor,
                           override val position: Vector,
                           override val inertia: Vector,
                           override val energy: Double,
                           override val time: Int,
                           foodAmount: Int) extends InsectInfo {

  override def updatePosition(newPosition: Vector): InsectInfo =
    ForagingAntInfo(proximitySensor,pheromoneSensor,newPosition,inertia,energy,time,foodAmount)

  override def updateInertia(newInertia: Vector): InsectInfo =
    ForagingAntInfo(proximitySensor,pheromoneSensor,position,newInertia,energy,time,foodAmount)

  override def updateEnergy(amount: Double): InsectInfo =
    if (energy + amount > MAX_ENERGY)
      ForagingAntInfo(proximitySensor,pheromoneSensor,position,inertia,MAX_ENERGY,time,foodAmount)
    else
      ForagingAntInfo(proximitySensor,pheromoneSensor,position,inertia,energy+amount,time,foodAmount)

  override def incTime(): InsectInfo =
    ForagingAntInfo(proximitySensor,pheromoneSensor,position,inertia,energy,time+1,foodAmount)

  def clearSensors(): InsectInfo =
    ForagingAntInfo(ProximitySensor(),PheromoneSensor(),position,inertia,energy,time,foodAmount)

  def addPheromones(pheromones: Iterable[Entity]): InsectInfo =
    ForagingAntInfo(updateSensor(pheromones,proximitySensor),PheromoneSensor(),position,inertia,energy,time,foodAmount)

  @scala.annotation.tailrec
  private def updateSensor( entities: Iterable[Entity], sensor: Sensor): Sensor =
    if (entities.isEmpty)
      sensor
    else
      updateSensor(entities.takeRight(entities.size - 1), sensor.addEntity(entities.take(1).last))

  def incFood(amount: Int): InsectInfo =
    if (foodAmount + amount > MAX_FOOD)
      ForagingAntInfo(proximitySensor,pheromoneSensor,position,inertia,energy,time,MAX_FOOD)
    else
      ForagingAntInfo(proximitySensor,pheromoneSensor,position,inertia,energy,time,foodAmount+amount)

  def freeFood(): InsectInfo = ForagingAntInfo(proximitySensor,pheromoneSensor,position,inertia,energy,time,STARTING_FOOD_AMOUNT)

}

object ForagingAntInfo {
  def apply(position: Vector = STARTING_POSITION, energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): ForagingAntInfo =
    new ForagingAntInfo(ProximitySensor(), PheromoneSensor(), position, ZeroVector2D(), energy, time, STARTING_FOOD_AMOUNT)
}


