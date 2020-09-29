package model.insects

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import model.environment.FoodPheromone
import model.insects.info.{AntInfo, ForagingAntInfo, SpecificInsectInfo}
import utility.geometry.Vectors._
import utility.Messages.{KillAnt, _}
import utility.geometry._
import utility.Parameters.ForagingAntConstant._

import scala.util.Random

object Constant {
  val NOISE = 0.1
  val FOOD_PHEROMONE_THRESHOLD: Double = 5.0
  val ANT_PHEROMONE_THRESHOLD: Double = 10.0
  val MAX_VELOCITY: Double = 5
  val MIN_VELOCITY: Double = 2
  val INERTIA_FACTOR: Double = 0.9
  val FOOD_EATEN_PER_STEP: Double = 0.5
  val ENERGY_RW: Double = -0.3
  val ENERGY_PF: Double = -0.2
  val ENERGY_EATING: Double = -0.1
  val ENERGY_SF: Double = -0.1
  val ENERGY_FPT: Double = -0.3
  val RANDOM: Random.type = scala.util.Random
}

import Constant._
import utility.PheromoneSeq._

/** A competence is the minimal building block to achieve a more complex behaviour. */
trait Competence[A <: SpecificInsectInfo[A]] {

  def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: A, behaviour: A => Receive): Unit

  /**
   * Check if this competence may be executed.
   *
   * @param info the insect's state.
   * @return true if this competence has priority over the all its less important competences.
   */
  def hasPriority(info: A): Boolean

}

/** Competence performing a random walk. */
case class RandomWalk[A <: SpecificInsectInfo[A]]() extends Competence[A] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: A, behaviour: A => Receive): Unit = {

    val data = info.updateEnergy(ENERGY_RW)
    val delta: Vector2D = RandomVector2DInCircle(MIN_VELOCITY, MAX_VELOCITY)
    val deltaWithInertia = OrientedVector2D((delta >> (info.inertia * INERTIA_FACTOR))./\, doubleInRange(MIN_VELOCITY, MAX_VELOCITY))
    environment.tell(Move(data.position, deltaWithInertia), ant)
    context become behaviour(data)
  }

  override def hasPriority(info: A): Boolean = true
}

/** Competence forcing an ant to go back to the anthill when its energy is low. */
case class GoBackToHome[A <: AntInfo[A]]() extends Competence[A] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: A, behaviour: A => Receive): Unit = {
    val data = info.updateEnergy(ENERGY_RW)
    info.anthill.tell(AntTowardsAnthill(info.position, MAX_VELOCITY, NOISE, info.isInsideTheAnthill), ant)
    context become behaviour(data)
  }

  override def hasPriority(info: A): Boolean = info.energy < 40 //TODO: to be parametrized, add new competence for carry food back to home
}

/** Competence forcing a foraging ant to go back to the anthill when its carrying food. */
case class CarryFoodToHome() extends Competence[ForagingAntInfo] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit =
    GoBackToHome[ForagingAntInfo]().apply(context,environment,ant,info,behaviour)

  override def hasPriority(info: ForagingAntInfo): Boolean = info.foodAmount > 0
}

/**
 * Competence forcing an insect to exit the anthill when its energy level is high.
 */
case class GoOutside[A <: AntInfo[A]]() extends Competence[A] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: A, behaviour: A => Receive): Unit = {
    val data = info.updateEnergy(ENERGY_RW).updateAnthillCondition(false)
    val delta: Vector2D = RandomVector2DInCircle(MIN_VELOCITY, MAX_VELOCITY)
    val deltaWithInertia = OrientedVector2D((delta >> (info.inertia * INERTIA_FACTOR))./\, doubleInRange(MIN_VELOCITY, MAX_VELOCITY))
    environment.tell(Move(data.position, deltaWithInertia), ant)
    context become behaviour(data)
  }

  override def hasPriority(info: A): Boolean = info.isInsideTheAnthill && info.energy > 80
  // info.isInsideTheAnthill && Random.nextDouble() * MAX_ENERGY < info.energy  //TODO: what if carrying food?
}

/**
 * Try to eat food from the anthill when the insect is inside it.
 */
case class EatFromTheAnthill[A <: AntInfo[A]]() extends Competence[A] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: A, behaviour: A => Receive): Unit = {
    info.anthill.tell(EatFood(FOOD_EATEN_PER_STEP), ant)
    val data = info.updateEnergy(ENERGY_EATING).updateInertia(ZeroVector2D())
    context become behaviour(data)
  }

  override def hasPriority(info: A): Boolean = info.isInsideTheAnthill
}

/**
 * Competence that enables ant to carry food when it find it.
 */
case class PickFood() extends Competence[ForagingAntInfo] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit = {
    environment.tell(TakeFood(MAX_FOOD - info.foodAmount, info.foodPosition.get), ant)
    context become behaviour(info.updateEnergy(ENERGY_PF))
  }

  override def hasPriority(info: ForagingAntInfo): Boolean = info.foodIsNear && info.foodAmount < MAX_FOOD
}

/**
 * An ant leaves the food in the anthill.
 */
case class StoreFoodInAnthill() extends Competence[ForagingAntInfo] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef,
                     info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit = {
    info.anthill.tell(StoreFood(info.foodAmount), ant)
    val data = info.freeFood().updateEnergy(ENERGY_SF)
    environment.tell(UpdateInsect(data), ant)
    context become behaviour(data)
  }

  override def hasPriority(info: ForagingAntInfo): Boolean = info.isInsideTheAnthill && info.foodAmount > 0
}

/**
 * When energy is 0 the insect dies. Must be the first competence for every insects.
 */
case class Die[A <: SpecificInsectInfo[A]]() extends Competence[A] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: A, behaviour: A => Receive): Unit =
    environment.tell(KillAnt(info.id), ant)

  override def hasPriority(info: A): Boolean = info.energy <= 0
}

/**
 * Competence that enable an ant to follow the traces of the (food) pheromone.
 */
case class FoodPheromoneTaxis() extends Competence[ForagingAntInfo] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit = {
    val delta = info.foodPheromones.toStream.filter(p => p.position --> info.position < ANT_PHEROMONE_THRESHOLD).weightedSum(info.position)
    val data = info.updateEnergy(ENERGY_FPT)
    val newDelta = OrientedVector2DWithNoise(delta./\, MAX_VELOCITY, NOISE) >> (data.inertia * 2)
    val newDelta2 = OrientedVector2D(newDelta./\, MAX_VELOCITY)
    environment.tell(Move(data.position, newDelta2), ant)
    context become behaviour(data.asInstanceOf[ForagingAntInfo].updateFoodPheromones(Seq.empty)) //TODO: should be correct also data without update
  }

  override def hasPriority(info: ForagingAntInfo): Boolean =
    info.foodPheromones.toStream.exists(p => p.position --> info.position < ANT_PHEROMONE_THRESHOLD)
}

import model.environment.FoodPheromoneInfo._

case class DropFoodPheromone() extends Competence[ForagingAntInfo] {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit = {
    environment.tell(AddFoodPheromone(FoodPheromone(info.position, DELTA, info.energy), FOOD_PHEROMONE_THRESHOLD), ant)
    val data = info.updateEnergy(ENERGY_RW)
    environment.tell(UpdateInsect(data), ant)
    context become behaviour(data)
  }

  override def hasPriority(info: ForagingAntInfo): Boolean =
    info.foodAmount > 0 && Random.nextDouble() < Math.pow(info.energy / MAX_ENERGY, 2)
}
