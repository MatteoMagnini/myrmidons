package model.insects

import akka.actor.{ActorContext, ActorRef}
import utility.Geometry.Vector2D
import utility.{Clock, InsectUpdate}
import scala.util.Random

trait Competence {

  def apply(context: ActorContext, environment: ActorRef, info: InsectInfo): Unit

  def hasPriority: Boolean

}

object RandomWalk extends Competence {

  val MAX_VELOCITY: Int = 5 * 2
  val NEGATIVE = 0.5
  val RANDOM: Random.type = scala.util.Random

  override def apply(context: ActorContext, environment: ActorRef, info: InsectInfo): Unit = {
    info.updatePosition(info.position + Vector2D((RANDOM.nextDouble() - NEGATIVE) * MAX_VELOCITY,
      (RANDOM.nextDouble() - NEGATIVE) * MAX_VELOCITY))
    environment ! InsectUpdate(info)
    environment ! Clock(info.time)
  }

  override def hasPriority: Boolean = false
}
