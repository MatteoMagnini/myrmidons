package model.insects

import utility.Geometry._

trait InsectInfo {

  val SEPARATOR = " "
  def position: Vector
  def energy: Double
  def time: Int

  def updatePosition(newPosition: Vector): Unit
  def updateEnergy(amount: Double): Unit
  def incTime(): Unit

  override def toString: String = position + SEPARATOR + energy + SEPARATOR + time + SEPARATOR + super.toString
}

object ForagingAntInfo extends InsectInfo {

  private val MAX_ENERGY = 100
  private val MAX_FOOD = 3
  var position: Vector = Vector2D(0,0)
  var energy: Double = 100
  var time: Int = 0
  var foodAmount: Int = 0

  def updatePosition(newPosition: Vector): Unit = position = newPosition

  //TODO: when 0 die!
  def updateEnergy(amount: Double): Unit = if (energy + amount > MAX_ENERGY) {
    energy = MAX_ENERGY
  } else if (energy + amount <=  0) {
    println("Insect should die!")
  } else {
    energy = energy + amount
  }

  override def incTime(): Unit = time = time + 1

  def incFood(amount: Int): Unit = if (foodAmount + amount > MAX_FOOD) foodAmount = MAX_FOOD else foodAmount = foodAmount + amount

  def freeFood(): Unit = foodAmount = 0

}


