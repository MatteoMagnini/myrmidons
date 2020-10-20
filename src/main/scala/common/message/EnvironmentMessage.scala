package common.message

import common.geometry.Vector2D
import common.rTree.RTree.Tree
import model.Drawable
import model.environment.pheromones.Pheromone

object EnvironmentMessage {

  /**
   * Message sent from environment to ants to update the pheromones that they perceive.
   *
   * @param pheromones pheromones
   * @param tree       optimal data structure to search near pheromone
   */
  case class Pheromones(pheromones: Map[Int, Pheromone], tree: Tree[Int]) extends Message

  /**
   * Message from environment to GUI, to repaint entities.
   *
   * @param info entities information
   */
  case class Repaint(info: Seq[Drawable]) extends Message

  /** When a Move message arrived.
   * Message sent from environment to insect, to share its new position.
   *
   * @param position insect new position
   * @param inertia  insect new inertia value
   */
  case class NewPosition(position: Vector2D, inertia: Vector2D) extends Message

  /**
   * Message sent from environment to GUI when all entities are initialized.
   */
  case object Ready extends Message

  /** When ant is near a food resource.
   * Message sent from environment to ant when it is near a food resource.
   *
   * @param foodPosition food position
   */
  case class FoodNear(foodPosition: Vector2D) extends Message

  /** When simulation start create ants.
   * Message sent from environment to anthill when simulation start.
   *
   * @param nAnts               number of ants to create
   * @param foragingProbability foraging ant birth probability
   */
  case class CreateAnts(nAnts: Int, foragingProbability: Double) extends Message

  /**
   * Message sent from environment to environment to check ant birth and its born.
   *
   * @param clock logic time
   */
  case class AntBirth(clock: Int) extends Message

}
