package model.insects.info

import common.geometry.Vector2D
import common.geometry.Vector2DFactory.ZeroVector2D
import model.insects.Enemies._

/** Enemy state.
  *
  * @param id       of the enemy
  * @param position of the enemy
  * @param inertia  of the enemy
  * @param energy   of the enemy
  * @param time     of simulation
  */
case class EnemyInfo(override val id: Int,
                     override val position: Vector2D,
                     override val inertia: Vector2D,
                     override val energy: Double,
                     override val time: Int) extends SpecificInsectInfo[EnemyInfo] {

  override def updatePosition(newPosition: Vector2D): EnemyInfo =
    this.copy(position = newPosition)

  override def updateInertia(newInertia: Vector2D): EnemyInfo =
    this.copy(inertia = newInertia)

  override def updateEnergy(delta: Double): EnemyInfo =
    this.copy(energy = if (energy + delta > MAX_ENERGY) MAX_ENERGY else energy + delta)

  override def incrementTime(): EnemyInfo =
    this.copy(time = time + 1)
}

object EnemyInfo {
  def apply(id: Int = 0, position: Vector2D = STARTING_POSITION,
            energy: Double = ENERGY, time: Int = STARTING_TIME): EnemyInfo =
    new EnemyInfo(id, position, ZeroVector2D(), energy, time)
}
