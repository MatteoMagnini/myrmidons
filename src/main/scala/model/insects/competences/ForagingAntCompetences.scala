package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import common.PheromoneSeq._
import common.RichActor._
import common.geometry.Vector2DFactory.{OrientedVector2D, OrientedVector2DWithNoise}
import common.message.InsectMessage._
import model.environment.pheromones.FoodPheromone
import model.environment.pheromones.FoodPheromoneInfo._
import model.insects.Ants.ForagingAnt._
import model.insects.info.ForagingAntInfo

/** Specific competences suitable only for foraging ants
 *
 */
trait ForagingAntCompetences extends AntCompetences[ForagingAntInfo]

/** Competence forcing a foraging ant to go back to the anthill when its carrying food.
 *
 * @param behaviour of the ant
 */
case class CarryFoodToHome(behaviour: ForagingAntInfo => Receive) extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo): Unit =
    GoBackToHome[ForagingAntInfo](behaviour).apply(context, environment, insect, info)

  override def hasPriority(info: ForagingAntInfo): Boolean = info.foodAmount > 0
}

/** Competence that enables foraging ants to carry food when it find it.
 *
 * @param behaviour of the ant
 */
case class PickFood(behaviour: ForagingAntInfo => Receive) extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo): Unit = {
    environment.tell(TakeFood(MAX_FOOD - info.foodAmount, info.foodPosition.get), insect)
    context >>> behaviour(info.updateEnergy(ENERGY_PICK_FOOD))
  }

  override def hasPriority(info: ForagingAntInfo): Boolean = info.foodIsNear && info.foodAmount < MAX_FOOD
}

/** A foraging ant leaves the food in the anthill.
 *
 * @param behaviour of the ant
 */
case class StoreFoodInAnthill(behaviour: ForagingAntInfo => Receive) extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo): Unit = {
    info.anthill.tell(StoreFood(info.foodAmount), insect)
    val data = info.freeFood().updateEnergy(ENERGY_STORE_FOOD)
    environment.tell(UpdateInsect(data), insect)
    context >>> behaviour(data)
  }

  override def hasPriority(info: ForagingAntInfo): Boolean = info.isInsideTheAnthill && info.foodAmount > 0
}

/** Competence that enable a foraging ant to follow the traces of (food) pheromones.
 *
 * @param behaviour of the ant
 */
case class FoodPheromoneTaxis(behaviour: ForagingAntInfo => Receive) extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo): Unit = {
    val delta = info.foodPheromones.toStream
      .filter(p => p.position --> info.position < FOOD_PHEROMONE_RANGE)
      .weightedSum(info.position)
    val data = info.updateEnergy(ENERGY_FOOD_PHEROMONE_TAXIS)
    val newDelta = OrientedVector2DWithNoise(delta./\, MAX_VELOCITY, NOISE) >> (data.inertia * INERTIA_FACTOR_IN_TAXIS)
    val newDelta2 = OrientedVector2D(newDelta./\, MAX_VELOCITY)
    environment.tell(Move(data.position, newDelta2), insect)
    context >>> behaviour(data.updateFoodPheromones(Seq.empty))
  }

  override def hasPriority(info: ForagingAntInfo): Boolean =
    info.foodPheromones.toStream.exists(p => p.position --> info.position < FOOD_PHEROMONE_RANGE)
}

/** A foraging ant drops food pheromones when going back to the anthill while carrying food.
 *
 * @param behaviour of the ant
 */
case class DropFoodPheromone(behaviour: ForagingAntInfo => Receive) extends ForagingAntCompetences {

  private def decreasingFunction: Double => Double = x => x * DECREASING_FACTOR - DELTA

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo): Unit = {
    environment.tell(AddPheromone(FoodPheromone(info.position, decreasingFunction, STARTING_INTENSITY),
      FOOD_PHEROMONE_MERGING_THRESHOLD), insect)
    val data = info.updateEnergy(ENERGY_RANDOM_WALK)
    environment.tell(UpdateInsect(data), insect)
    context >>> behaviour(data)
  }

  override def hasPriority(info: ForagingAntInfo): Boolean =
    info.foodAmount > 0 && random(Math.pow(info.energy / MAX_ENERGY, 2))
}
