package model.insects.info

import akka.actor.ActorRef

/**
 * The information in common with all kind of ants.
 *
 * @tparam A the specific type of ant
 */
trait AntInfo[A <: AntInfo[A]] extends SpecificInsectInfo[A] {

  /**
   * @return the reference of the anthill
   */
  def anthill: ActorRef

  /**
   * @return true if the ant is inside the anthill, false otherwise
   */
  def isInsideTheAnthill: Boolean

  /**
   * @param value the new condition
   * @return a new AntInfo with the updated anthill condition
   */
  def antEntersAnthill(value: Boolean): A

}
