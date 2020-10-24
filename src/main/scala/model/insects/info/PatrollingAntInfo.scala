package model.insects.info

import akka.actor.ActorRef
import common.geometry.Vector2D
import common.geometry.Vector2DFactory.ZeroVector2D
import model.environment.pheromones.DangerPheromone
import model.insects.Ants.PatrollingAnt._

/**
 * This class defines a patrolling ant state.
 *
 * @param id the ant identifier
 * @param position of the ant
 * @param inertia of the ant
 * @param energy of the ant
 * @param time of simulation
 * @param anthill reference
 * @param isInsideTheAnthill condition
 * @param dangerPheromones perceived
 */
case class PatrollingAntInfo(override val id: Int,
                        override val position: Vector2D,
                        override val inertia: Vector2D,
                        override val energy: Double,
                        override val time: Int,
                        override val anthill: ActorRef,
                        override val isInsideTheAnthill: Boolean,
                        dangerPheromones: Seq[DangerPheromone] )  extends AntInfo[PatrollingAntInfo]  {

  /**
    * @param value the new condition
    * @return a new AntInfo with the updated anthill condition
    */
  override def antEntersAnthill(value: Boolean): PatrollingAntInfo =
    this.copy(isInsideTheAnthill = value)

  /**
    * @param newPosition the new position
    * @return a new InsectInfo with the updated position
    */
  override def updatePosition(newPosition: Vector2D): PatrollingAntInfo =
    this.copy(position = newPosition)

  /**
    * @param newInertia the new inertia
    * @return a new InsectInfo with the updated inertia
    */
  override def updateInertia(newInertia: Vector2D): PatrollingAntInfo =
    this.copy(inertia = newInertia)

  /**
    * @param delta to be added (can be negative)
    * @return a new InsectInfo with the updated energy
    */
  override def updateEnergy(delta: Double): PatrollingAntInfo =
    this.copy(energy = if (energy + delta > MAX_ENERGY) MAX_ENERGY else energy + delta)

  /**
    * @return a new InsectInfo with the incremented time
    */
  override def incrementTime(): PatrollingAntInfo =
    this.copy(time = time + 1)

  /**
   * @param pheromones the sequence of danger pheromones in the environment
   * @return a new PatrollingAntInfo with the updated danger pheromones
   */
  def updateDangerPheromones(pheromones: Seq[DangerPheromone]): PatrollingAntInfo =
    this.copy(dangerPheromones = pheromones)
}

object PatrollingAntInfo {
  def apply( anthill: ActorRef, id: Int = 0, position: Vector2D = STARTING_POSITION,
             energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): PatrollingAntInfo =
    PatrollingAntInfo(id, position, ZeroVector2D(), energy, time, anthill, isInsideTheAnthill = false, Seq.empty)
}
