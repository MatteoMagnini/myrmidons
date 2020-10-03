package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import model.insects.info.AntInfo
import utility.Messages.{AntTowardsAnthill, EatFood, Move}
import utility.geometry.Vectors._
import utility.geometry._
import utility.Parameters.Competence._

/**
 * Competence for all ants.
 *
 * @tparam A th type of the ant
 */
trait AntCompetences[A <: AntInfo[A]] extends InsectCompetences[A]

/**
 * Competence forcing an ant to go back to the anthill when its energy is low.
 */
case class GoBackToHome[A <: AntInfo[A]]() extends AntCompetences[A] {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A, behaviour: A => Receive): Unit = {
    val data = info.updateEnergy(ENERGY_RW)
    data.anthill.tell(AntTowardsAnthill(data.position, MAX_VELOCITY, data.inertia, NOISE, info.isInsideTheAnthill), insect)
    context >>> behaviour(data)
  }

  override def hasPriority(info: A): Boolean = info.energy < 40 //TODO: to be parametrized, add new competence for carry food back to home
}

/**
 * Competence forcing an insect to exit the anthill when its energy level is high.
 */
case class GoOutside[A <: AntInfo[A]]() extends AntCompetences[A] {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A, behaviour: A => Receive): Unit = {
    val data = info.updateEnergy(ENERGY_RW).updateAnthillCondition(false)
    val delta: Vector2D = RandomVector2DInCircle(MIN_VELOCITY, MAX_VELOCITY)
    val deltaWithInertia = OrientedVector2D((delta >> (info.inertia * INERTIA_FACTOR))./\, doubleInRange(MIN_VELOCITY, MAX_VELOCITY))
    environment.tell(Move(data.position, deltaWithInertia), insect)
    context >>> behaviour(data)
  }

  override def hasPriority(info: A): Boolean = info.isInsideTheAnthill && info.energy > 80 //TODO: try something with probability
}

/**
 * Eat food from the anthill (if present) when the insect is inside it.
 */
case class EatFromTheAnthill[A <: AntInfo[A]]() extends AntCompetences[A] {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A, behaviour: A => Receive): Unit = {
    info.anthill.tell(EatFood(FOOD_EATEN_PER_STEP), insect)
    val data = info.updateEnergy(ENERGY_EATING).updateInertia(ZeroVector2D())
    context >>> behaviour(data)
  }

  override def hasPriority(info: A): Boolean = info.isInsideTheAnthill
}