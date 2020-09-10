package model.insects

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import utility.Geometry._
import utility.Messages._

import scala.util.Random

object Constant {
  val MAX_VELOCITY: Int = 5
  val MIN_VELOCITY: Int = 1
  val INERTIA_FACTOR: Double = 0.9
  val ENERGY_RW: Int = - 1
  val ENERGY_FPT: Double = - 1.5
  val RANDOM: Random.type = scala.util.Random
}

import Constant._

/**
 * A competence is the minimal building block to achieve a more complex behaviour.
 */
trait Competence {

  def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit

  /**
   * Check if this competence may be executed.
   * @param info the insect's state.
   * @return true if this competence has priority over the all its less important competences.
   */
  def hasPriority(info: InsectInfo): Boolean

}

/**
 * Competence performing a random walk.
 */
object RandomWalk extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {

    val data = info.updateEnergy(ENERGY_RW)
    environment.tell(Move(data.position, RandomVector2D(MAX_VELOCITY, MIN_VELOCITY,(info.inertia * INERTIA_FACTOR))),ant)
    context become behaviour(data)
  }

  override def hasPriority(info: InsectInfo): Boolean = true
}

/**
 * Competence that enable an ant to follow the traces of the (food) pheromone.
 */
object FoodPheromoneTaxis extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    val delta = info.asInstanceOf[ForagingAntInfo].pheromoneSensor.weightedSum
    val data = info.updateEnergy(ENERGY_FPT)
    environment.tell(Move(data.position, delta),ant)
    context become behaviour(data.asInstanceOf[ForagingAntInfo].clearSensors())
  }

  override def hasPriority(info: InsectInfo): Boolean =
    info.asInstanceOf[ForagingAntInfo].pheromoneSensor.strongest.nonEmpty
}