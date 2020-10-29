package model.environment.fights

import common.geometry.Vector2D
import model.Drawable
import model.insects.info.{EnemyInfo, InsectInfo}

/** Dead insect in a fight */
sealed trait DeadInsect extends Drawable {
  def insect: InsectInfo
}

case class DeadAnt(override val insect: InsectInfo, override val position: Vector2D) extends DeadInsect

case class DeadEnemy(override val insect: EnemyInfo, override val position: Vector2D) extends DeadInsect

