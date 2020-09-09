package model.insects

import model.Vector2D

/**
 * The information in common with all kind of insects.
 */
trait InsectInfo {

  private val SEPARATOR = " "
  def MAX_ENERGY = 100
  def MAX_FOOD = 3
  def STARTING_ENERGY = 100
  def STARTING_TIME = 0
  def STARTING_FOOD_AMOUNT = 0
  def STARTING_POSITION: Vector2D = Vector2D(0,0)

  def position: Vector2D
  def energy: Double
  def time: Int

  def updatePosition(newPosition: Vector2D): Unit
  def updateEnergy(amount: Double): Unit
  def incTime(): Unit

  override def toString: String = position + SEPARATOR + energy + SEPARATOR + time + SEPARATOR + super.toString
}

case class ForagingAntInfo() extends InsectInfo {

  val proximitySensor: Sensor = ProximitySensor()
  val pheromoneSensor: Sensor = PheromoneSensor()

  var position: Vector2D = STARTING_POSITION
  var energy: Double = STARTING_ENERGY
  var time: Int = STARTING_TIME
  var foodAmount: Int = STARTING_FOOD_AMOUNT

  override def updatePosition(newPosition: Vector2D): Unit = position = newPosition

  override def updateEnergy(amount: Double): Unit =
    if (energy + amount > MAX_ENERGY) energy = MAX_ENERGY else energy = energy + amount

  override def incTime(): Unit = time = time + 1

  def incFood(amount: Int): Unit =
    if (foodAmount + amount > MAX_FOOD) foodAmount = MAX_FOOD else foodAmount = foodAmount + amount

  def freeFood(): Unit = foodAmount = 0

}


