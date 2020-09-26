package model.insects

import akka.actor.ActorRef
import model.Drawable
import utility.Geometry._

/** The information in common with all kind of insects. */
trait InsectInfo extends Drawable with Product with Serializable {

  def id: Int

  def inertia: Vector2D

  def energy: Double

  def time: Int

  def anthill: ActorRef

  def isInsideTheAnthill: Boolean

  def foodPosition: Option[Vector2D]

  def updatePosition(newPosition: Vector2D): InsectInfo

  def updateInertia(newInertia: Vector2D): InsectInfo

  def updateEnergy(amount: Double): InsectInfo

  def incTime(): InsectInfo

  def updateAnthillCondition(value: Boolean): InsectInfo

  def foodIsNear: Boolean = foodPosition.nonEmpty

  def updateFoodPosition(position: Option[Vector2D]): InsectInfo
}

