package utility

import model.Drawable
import model.anthill.AnthillInfo

import model.environment.{FoodPheromone, Pheromone}
import model.insects.InsectInfo
import utility.Geometry.Vector2D


sealed trait Message

object Messages {

  /** Message sent from GUI to environment, to start simulation.
   *
   * @param nAnts       number of ants to be created
   * @param centerSpawn whether spawn ants from center of boundaries
   */
  case class StartSimulation(nAnts: Int, nEnemies: Int, centerSpawn: Boolean = false, obstacles: Option[Int] = Some(6), food: Option[Int] = Some(6)) extends Message

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

  case class FoodPheromones(pheromones: Seq[FoodPheromone]) extends Message

  case class AddFoodPheromone(foodPheromone: FoodPheromone, threshold: Double) extends Message

  /** Message sent from ant to environment, to update its information.
   *
   * @param info ant information
   */
  case class UpdateInsect(info: InsectInfo) extends Message

  /** Message from environment to GUI, to repaint ants.
   *
   * @param info ants information
   */
  case class Repaint(info: Iterable[Drawable]) extends Message

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

  /**
   * An ant has a bit of memory (it counts its steps).
   * The memory is emulated by asking the anthill actor to send the resulting movement to perform.
   *
   * @param position the ant position
   * @param maxSpeed ant max velocity
   * @param antIsIn  true if it is inside the anthill, false otherwise
   */
  case class AntTowardsAnthill(position: Vector2D, maxSpeed: Double, noise: Double, antIsIn: Boolean) extends Message

  case class FoodNear(foodPosition: Vector2D) extends Message

  /**
   * @param antIsInsideTheAnthill true if the ant is inside, false otherwise
   */
  case class UpdateAnthillCondition(antIsInsideTheAnthill: Boolean) extends Message

  /**
   * Message from GUI to create new ants with RandomPosition.
   *
   * @param nAnts number of ants to create
   * @param step  number of logical clock when user click button
   */
  case class AddRandomAnt(nAnts: Int, step: String) extends Message

  case class AntBirth(clock: Int) extends Message

  case class KillAnt(id: Int) extends Message

}
