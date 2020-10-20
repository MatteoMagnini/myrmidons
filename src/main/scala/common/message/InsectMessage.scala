package common.message

import common.geometry.Vector2D
import model.environment.pheromones.Pheromone
import model.insects.info.InsectInfo

object InsectMessage {

  /** When an insect die.
   * Message sent from insect to environment when insect energy is zero.
   * Message sent from environment to insect when it loose a fight.
   *
   * @param info insect information
   */
  case class KillInsect(info: InsectInfo) extends Message


  /** When insect update is needs.
   * Message sent from insect to environment when insect information change.
   *
   * @param info ant information
   */
  case class UpdateInsect(info: InsectInfo) extends Message

  /** When insect ask environment to move.
   * Message sent from insect to environment when it want to move.
   *
   * @param start initial position
   * @param delta shift vector
   */
  case class Move(start: Vector2D, delta: Vector2D) extends Message

  /** When ant drop pheromone.
   * Message sent form ants to environment when they drop pheromone in simulation space.
   *
   * @param pheromone pheromone type to drop.
   * @param threshold threshold for merge pheromone if is near pheromone.
   */
  case class AddPheromone(pheromone: Pheromone, threshold: Double) extends Message

  /** When ant takes food from food source.
   * Message sent from ant to environment when it want to take food from food source.
   * Message sent from environment to ant to approve the food action.
   *
   * @param delta    taken food quantity
   * @param position food position
   */
  case class TakeFood(delta: Double, position: Vector2D) extends Message

  /** When ant store food in the anthill.
   * Message sent from ant to anthill when it want to deposit food.
   *
   * @param delta stored food quantity
   */
  case class StoreFood(delta: Double) extends Message

  /** When ant eat food from the anthill.
   * Message sent from ant to anthill when it eat food.
   *
   * @param delta food amount
   */
  case class EatFood(delta: Double) extends Message


  /** When ant want to back home.
   * Message sent from ant to anthill when it want to return to home.
   *
   * @param position the ant position
   * @param maxSpeed ant max velocity
   * @param inertia  ant inertia
   * @param noise    to avoid getting stacked
   * @param antIsIn  true if it is inside the anthill, false otherwise
   */
  case class AntTowardsAnthill(position: Vector2D,
                               maxSpeed: Double,
                               inertia: Vector2D,
                               noise: Double,
                               antIsIn: Boolean) extends Message

}
