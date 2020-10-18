package common

import akka.actor.ActorContext
import model.Drawable
import model.environment.anthill.AnthillInfo
import model.environment.data.InsectReferences
import model.environment.pheromones.Pheromone
import common.geometry.Vector2D
import model.insects.info.InsectInfo
import common.rTree.RTree.Tree
import common.rTree.RTreeProlog

trait Message

object Messages {

  val DEFAULT_OBSTACLES = 6
  val DEFAULT_FOOD = 6
  val DEFAULT_ANTHILL_FOOD = 200

  /** Message sent from GUI to environment, to start simulation.
   *
   * @param nAnts number of ants to be created
   */
  case class StartSimulation(nAnts: Int,
                             nEnemies: Int,
                             obstacles: Option[Int] = Some(DEFAULT_OBSTACLES),
                             food: Option[Int] = Some(DEFAULT_FOOD),
                             anthillFood: Option[Int] = Some(DEFAULT_ANTHILL_FOOD)) extends Message


  /** Message sent from GUI to environment and from environment to ants, to do a step in simulation.
   *
   * @param value clock value
   */
  case class Clock(value: Int) extends Message

  /** Message sent from ant to environment, to change position.
   *
   * @param start initial position
   * @param delta shift vector
   */
  case class Move(start: Vector2D, delta: Vector2D) extends Message

  case class Pheromones(pheromones: Map[Int,Pheromone], tree: Tree[Int]) extends Message

  case class AddPheromone(foodPheromone: Pheromone, threshold: Double) extends Message

  /** Message sent from ant to environment, to update its information.
   *
   * @param info ant information
   */
  case class UpdateInsect(info: InsectInfo) extends Message

  /** Message from environment to GUI, to repaint ants.
   *
   * @param info ants information
   */
  case class Repaint(info: Seq[Drawable]) extends Message

  /** Message sent from environment to ant, to share its new position.
   *
   * @param position ant new position
   * @param inertia  ant new inertia value
   */
  case class NewPosition(position: Vector2D, inertia: Vector2D) extends Message

  case class TakeFood(delta: Double, position: Vector2D) extends Message

  case class StoreFood(delta: Double) extends Message

  case class EatFood(delta: Double) extends Message

  case class UpdateAnthill(info: AnthillInfo) extends Message

  case object Ready extends Message
  /**
   * An ant has a bit of memory (it counts its steps).
   * The memory is emulated by asking the anthill actor to send the resulting movement to perform.
   *
   * @param position the ant position
   * @param maxSpeed ant max velocity
   * @param inertia ant inertia
   * @param noise to avoid getting stacked
   * @param antIsIn  true if it is inside the anthill, false otherwise
   */
  case class AntTowardsAnthill(position: Vector2D,
                               maxSpeed: Double,
                               inertia: Vector2D,
                               noise: Double,
                               antIsIn: Boolean) extends Message

  case class FoodNear(foodPosition: Vector2D) extends Message

  /**
   * @param antIsInsideTheAnthill true if the ant is inside, false otherwise
   */
  case class UpdateAnthillCondition(antIsInsideTheAnthill: Boolean) extends Message

  case class CreateEntities(nAnts: Int, foragingProbability: Double) extends Message

  case class NewEntities(ants: InsectReferences) extends Message
  /**
   * Message from GUI to create new ants with RandomPosition.
   *
   * @param nAnts number of ants to create
   * @param step  number of logical clock when user click button
   */
  case class AddRandomAnt(nAnts: Int, step: String) extends Message

  case class AntBirth(clock: Int) extends Message

  case class KillInsect(info: InsectInfo) extends Message

  //TODO: just for test!
  case class Context(context: Option[ActorContext]) extends Message
}
