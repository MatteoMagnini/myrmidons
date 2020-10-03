package model.insects.info
import akka.actor.ActorRef
import utility.Parameters.ForagingAnt._
import utility.geometry.{Vector2D, ZeroVector2D}

case class PatrollingAntInfo(override val id: Int,
                        override val position: Vector2D,
                        override val inertia: Vector2D,
                        override val energy: Double,
                        override val time: Int,
                        override val anthill: ActorRef,
                        override val isInsideTheAnthill: Boolean)  extends AntInfo[PatrollingAntInfo]  {

  /**
    * @param value the new condition
    * @return a new AntInfo with the updated anthill condition
    */
  override def updateAnthillCondition(value: Boolean): PatrollingAntInfo =
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
  override def incTime(): PatrollingAntInfo =
    this.copy(time = time + 1)
}


object PatrollingAntInfo {
  def apply( anthill: ActorRef, id: Int = 0, position: Vector2D = STARTING_POSITION, energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): PatrollingAntInfo =
    PatrollingAntInfo(id, position, ZeroVector2D(), energy, time, anthill, false)
}
