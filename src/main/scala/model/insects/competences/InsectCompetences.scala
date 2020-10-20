package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import model.insects.info.SpecificInsectInfo
import common.geometry.Vectors._
import common.geometry._
import common.RichActor._
import common.message.InsectMessage.{KillInsect, Move}

/**
 * A competence is the minimal building block to achieve a more complex behaviour.
 * An insect competence is the more general one. Every type of insect can perform it.
 *
 * @tparam A the type of the insect
 */
trait InsectCompetences[A <: SpecificInsectInfo[A]] {

  /**
   * @return behaviour of the insect
   */
  def behaviour: A => Receive

  /**
   * Execute the competence.
   * @param context of the insect actor
   * @param environment of the simulation
   * @param insect that perform the competence
   * @param info the state of the insect
   */
  def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A): Unit

  /**
   * Check if this competence may be executed.
   *
   * @param info the insect's state.
   * @return true if this competence has priority over the all its less important competences.
   */
  def hasPriority(info: A): Boolean

}

/**
 * Competence performing a random walk.
 *
 * @param behaviour of the insect
 * @tparam A the type of the insect
 */
case class RandomWalk[A <: SpecificInsectInfo[A]](behaviour: A => Receive) extends InsectCompetences[A] {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A): Unit = {
    val data = info.updateEnergy(ENERGY_RANDOM_WALK)
    val delta: Vector2D = RandomVector2DInCircle(MIN_VELOCITY, MAX_VELOCITY)
    val deltaWithInertia = OrientedVector2D((delta >> (info.inertia * INERTIA_FACTOR))./\,
      doubleInRange(MIN_VELOCITY, MAX_VELOCITY))
    environment.tell(Move(data.position, deltaWithInertia), insect)
    context >>> behaviour(data)
  }

  override def hasPriority(info: A): Boolean = true
}

/**
 * When energy is 0 the insect dies. Must be the first competence for every insects.
 *
 * @param behaviour of the insect
 * @tparam A the type of the insect
 */
case class Die[A <: SpecificInsectInfo[A]](behaviour: A => Receive) extends InsectCompetences[A] {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: A): Unit =
    environment.tell(KillInsect(info), insect)

  override def hasPriority(info: A): Boolean = info.energy <= 0
}
