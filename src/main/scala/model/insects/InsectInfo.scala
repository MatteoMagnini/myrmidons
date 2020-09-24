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


/** The information in common with all kind of insects. */
trait InsectInfo extends Drawable with Product with Serializable {

  def id: Int
  def inertia: Vector2D
  def energy: Double
  def time: Int
  def anthill: ActorRef
  def isInsideTheAnthill: Boolean

  def updatePosition(newPosition: Vector2D): InsectInfo
  def updateInertia(newInertia: Vector2D): InsectInfo
  def updateEnergy(amount: Double): InsectInfo
  def incTime(): InsectInfo
  def updateAnthillCondition(value: Boolean): InsectInfo
}



