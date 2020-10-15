package model.insects.info

import akka.actor.ActorRef
import model.environment.pheromones.FoodPheromone
import model.insects.Ants.ForagingAnt._
import utility.geometry.{Vector2D, ZeroVector2D}

/**
 * This class defines a foraging ant state.
 *
 * @param id the ant identifier
 * @param position of the ant
 * @param inertia of the ant
 * @param energy of the ant
 * @param time of simulation
 * @param anthill reference
 * @param isInsideTheAnthill condition
 * @param foodAmount carried
 * @param foodPosition perceived
 * @param foodPheromones perceived
 */
case class ForagingAntInfo(override val id: Int,
                           override val position: Vector2D,
                           override val inertia: Vector2D,
                           override val energy: Double,
                           override val time: Int,
                           override val anthill: ActorRef,
                           override val isInsideTheAnthill: Boolean,
                           foodAmount: Double,
                           foodPosition: Option[Vector2D],
                           foodPheromones: Seq[FoodPheromone]) extends AntInfo[ForagingAntInfo] {

  override def updatePosition(newPosition: Vector2D): ForagingAntInfo =
    this.copy(position = newPosition)

  override def updateInertia(newInertia: Vector2D): ForagingAntInfo =
    this.copy(inertia = newInertia)

  override def updateEnergy(delta: Double): ForagingAntInfo =
    this.copy(energy = if (energy + delta > MAX_ENERGY) MAX_ENERGY else energy + delta)

  override def incTime(): ForagingAntInfo =
    this.copy(time = time + 1)

  override def updateAnthillCondition(value: Boolean): ForagingAntInfo =
    this.copy(isInsideTheAnthill = value)

  /**
   * @param delta to be added
   * @return a new ForagingAntInfo with the updated food amount
   */
  def incFood(delta: Double): ForagingAntInfo =
    this.copy(foodAmount = if (foodAmount + delta > MAX_FOOD) MAX_FOOD else foodAmount + delta)

  /**
   * @return a new ForagingAntInfo with the zero food amount
   */
  def freeFood(): ForagingAntInfo =
    this.copy(foodAmount = STARTING_FOOD_AMOUNT)

  /**
   * @param position optional new position, None if there is no food nearby,
   *                 otherwise Some(p) where p is the food position
   * @return a new ForagingAntInfo with the updated food position
   */
  def updateFoodPosition(position: Option[Vector2D]): ForagingAntInfo =
    this.copy(foodPosition = position)

  def foodIsNear: Boolean = foodPosition.nonEmpty

  /**
   * @param pheromones the sequence of food pheromones in the environment
   * @return a new ForagingAntInfo with the updated food pheromones
   */
  def updateFoodPheromones(pheromones: Seq[FoodPheromone]): ForagingAntInfo =
    this.copy(foodPheromones = pheromones)

}

object ForagingAntInfo {
  def apply( anthill: ActorRef, id: Int = 0, position: Vector2D = STARTING_POSITION,
             energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): ForagingAntInfo =
    new ForagingAntInfo(id, position, ZeroVector2D(), energy, time, anthill,
      isInsideTheAnthill =  false, STARTING_FOOD_AMOUNT, None, Seq.empty)
}
