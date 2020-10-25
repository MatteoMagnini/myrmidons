package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import common.RichActor._
import common.geometry.Vector2DFactory.{OrientedVector2D, RandomVector2DInCircle, ZeroVector2D}
import common.geometry.Vectors._
import common.geometry._
import common.message.InsectMessage.{AntTowardsAnthill, EatFood, Move}
import model.insects.info.AntInfo

/** Competence for all ants.
 *
 * @tparam A th type of the ant
 */
trait AntCompetences[A <: AntInfo[A]] extends InsectCompetences[A]

/** Competence forcing an ant to go back to the anthill when its energy is low.
 *
 * @param behaviour of the ant
 * @tparam A th type of the ant
 */
case class GoBackToHome[A <: AntInfo[A]](behaviour: A => Receive) extends AntCompetences[A] {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A): Unit = {
    val data = info.updateEnergy(ENERGY_RANDOM_WALK)
    data.anthill.tell(AntTowardsAnthill(data.position, MAX_VELOCITY,
      data.inertia, NOISE, info.isInsideTheAnthill), insect)
    context >>> behaviour(data)
  }

  override def hasPriority(info: A): Boolean = info.energy < THRESHOLD_GO_BACK_HOME
}

/** Competence forcing an insect to exit the anthill when its energy level is high.
 *
 * @param behaviour of the ant
 * @tparam A th type of the ant
 */
case class GoOutside[A <: AntInfo[A]](behaviour: A => Receive) extends AntCompetences[A] {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A): Unit = {
    val data = info.updateEnergy(ENERGY_RANDOM_WALK).antEntersAnthill(value = false)
    val delta: Vector2D = RandomVector2DInCircle(MIN_VELOCITY, MAX_VELOCITY)
    val deltaWithInertia = OrientedVector2D((delta >> (info.inertia * INERTIA_FACTOR))./\,
      doubleInRange(MIN_VELOCITY, MAX_VELOCITY))
    environment.tell(Move(data.position, deltaWithInertia), insect)
    context >>> behaviour(data)
  }

  override def hasPriority(info: A): Boolean =
    info.isInsideTheAnthill && info.energy > THRESHOLD_GO_OUTSIDE && random(probability = 0.5)
}

/** Eat food from the anthill (if present) when the insect is inside it.
 *
 * @param behaviour of the ant
 * @tparam A th type of the ant
 */
case class EatFromTheAnthill[A <: AntInfo[A]](behaviour: A => Receive) extends AntCompetences[A] {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A): Unit = {
    info.anthill.tell(EatFood(FOOD_EATEN_PER_STEP), insect)
    val data = info.updateEnergy(ENERGY_EATING).updateInertia(ZeroVector2D())
    context >>> behaviour(data)
  }

  override def hasPriority(info: A): Boolean = info.isInsideTheAnthill
}
