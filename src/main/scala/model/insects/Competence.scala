package model.insects

import akka.actor.{ActorContext, ActorRef}
import utility.Geometry.Vector2D
import utility.{Clock, InsectUpdate}
import scala.util.Random

trait Competence {

  val MAX_VELOCITY: Int = 5 * 2
  val NEGATIVE = 0.5
  val RANDOM: Random.type = scala.util.Random

  def apply(context: ActorContext, environment: ActorRef, info: InsectInfo): Unit

  def hasPriority(info: InsectInfo): Boolean

}

object RandomWalk extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, info: InsectInfo): Unit = {
    info.updatePosition(info.position + Vector2D((RANDOM.nextDouble() - NEGATIVE) * MAX_VELOCITY,
      (RANDOM.nextDouble() - NEGATIVE) * MAX_VELOCITY))
    environment ! InsectUpdate(info)
    environment ! Clock(info.time)
  }

  override def hasPriority(info: InsectInfo): Boolean = true
}

object FoodPheromoneTaxis extends Competence {

  override def apply( context: ActorContext, environment: ActorRef, info: InsectInfo ): Unit = {
    info.updatePosition(info.position + info.asInstanceOf[ForagingAntInfo].pheromoneSensor.weightedSum)
    info.asInstanceOf[ForagingAntInfo].pheromoneSensor.clearEntity()
    environment ! InsectUpdate(info)
    environment ! Clock(info.time)
  }

  override def hasPriority(info: InsectInfo): Boolean =
    info.asInstanceOf[ForagingAntInfo].pheromoneSensor.strongest.nonEmpty
}