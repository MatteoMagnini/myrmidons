package model.insects.info

import utility.Parameters.ForagingAntConstant._
import utility.geometry.{Vector2D, ZeroVector2D}

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

  override def incTime(): EnemyInfo =
    this.copy(time = time + 1)
}

object EnemyInfo {
  def apply(id: Int = 0, position: Vector2D = STARTING_POSITION, energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): EnemyInfo =
    new EnemyInfo(id, position, ZeroVector2D(), energy, time)
}
