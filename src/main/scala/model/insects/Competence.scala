package model.insects

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import utility.Geometry._
import utility.Messages.{AntTowardsAnthill, Move, TakeFood}

import scala.util.Random

object Constant {
  val MAX_VELOCITY: Double = 5
  val MIN_VELOCITY: Double = - 5
  val INERTIA_FACTOR: Double = 0.9
  val FOOD_EATEN_PER_STEP: Double = 0.5
  val ENERGY_RW: Double = - 0.3
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
    val delta: Vector2D = RandomVector2D(MIN_VELOCITY, MAX_VELOCITY, info.inertia * INERTIA_FACTOR)
    environment.tell(Move(data.position, delta),ant)
    context become behaviour(data)
  }

  override def hasPriority(info: InsectInfo): Boolean = true
}

/**
  * Competence forcing an ant to go back to the anthill when its energy is low.
  */
object GoBackToHome extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive ): Unit = {
    val data = info.updateEnergy(ENERGY_RW)
    info.anthill.tell(AntTowardsAnthill(info.position, MAX_VELOCITY, info.isInsideTheAnthill),ant)
    context become behaviour(data)
  }

  override def hasPriority( info: InsectInfo ): Boolean = info.energy < 40 //TODO: clearly to be parametrized
}

object EatFromTheAnthill extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    println(s"Ant ${info.id} eating from the anthill with energy ${info.energy}")
    info.anthill.tell(TakeFood(FOOD_EATEN_PER_STEP),ant)
    context become behaviour(info)
  }

  override def hasPriority(info: InsectInfo): Boolean = info.isInsideTheAnthill && info.energy < 80 //TODO: clearly to be parametrized
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

  override def hasPriority(info: InsectInfo): Boolean = info match {
    case f: ForagingAntInfo => f.pheromoneSensor.entities.nonEmpty
    case _ => false
  }
}