package model.insects.info

import model.Drawable
import utility.geometry.Vector2D

trait InsectInfo extends Drawable {

  /**
   * @return the insect identifier
   */
  def id: Int

  /** position is taken from drawable */

  /**
   * @return the inertia of the insect
   */
  def inertia: Vector2D

  /**
   * @return the energy of the insect
   */
  def energy: Double

  /**
   * @return the time of the insect that should be equal to the time of the simulation
   */
  def time: Int

}

/** The information in common with all kind of insects plus the specialized type carried. */
trait SpecificInsectInfo[A <: SpecificInsectInfo[A]] extends InsectInfo {

  /**
   * @param newPosition the new position
   * @return a new InsectInfo with the updated position
   */
  def updatePosition(newPosition: Vector2D): A

  /**
   * @param newInertia the new inertia
   * @return a new InsectInfo with the updated inertia
   */
  def updateInertia(newInertia: Vector2D): A

  /**
   * @param delta to be added (can be negative)
   * @return a new InsectInfo with the updated energy
   */
  def updateEnergy(delta: Double): A

  /**
   * @return a new InsectInfo with the incremented time
   */
  def incTime(): A
}

