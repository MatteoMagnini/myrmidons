package model.insects

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import utility.Geometry._
import utility.Messages._

import scala.util.Random

trait Competence {

  val MAX_VELOCITY: Int = 5
  val MIN_VELOCITY: Int = 1
  val ENERGY_RW: Int = - 1
  val ENERGY_FPT: Double = - 1.5
  val RANDOM: Random.type = scala.util.Random

  def apply(context: ActorContext, environment: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit

  def hasPriority(info: InsectInfo): Boolean

}

object RandomWalk extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    val data = info.updateEnergy(ENERGY_RW)
    println("Random walk")
    environment ! Move(data.position, RandomVector2D(MAX_VELOCITY, MIN_VELOCITY))
    context become behaviour(data)
  }

  override def hasPriority(info: InsectInfo): Boolean = true
}

object FoodPheromoneTaxis extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    val delta = info.asInstanceOf[ForagingAntInfo].pheromoneSensor.weightedSum
    val data = info.updateEnergy(ENERGY_FPT)
    environment ! Move(data.position, delta)
    context become behaviour(data.asInstanceOf[ForagingAntInfo].clearSensors())
  }

  override def hasPriority(info: InsectInfo): Boolean =
    info.asInstanceOf[ForagingAntInfo].pheromoneSensor.strongest.nonEmpty
}