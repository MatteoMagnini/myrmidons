package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import model.environment.FoodPheromone
import model.insects.info.ForagingAntInfo
import utility.Messages._
import utility.PheromoneSeq._
import model.environment.FoodPheromoneInfo._
import utility.geometry._
import utility.Parameters.Competence._
import utility.Parameters.ForagingAnt._
import scala.util.Random

/**
 * Specific competences suitable only for foraging ants
 */
trait ForagingAntCompetences extends InsectCompetences[ForagingAntInfo]

/**
 * Competence forcing a foraging ant to go back to the anthill when its carrying food.
 */
case class CarryFoodToHome() extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit =
    GoBackToHome[ForagingAntInfo]().apply(context,environment,insect,info,behaviour)

  override def hasPriority(info: ForagingAntInfo): Boolean = info.foodAmount > 0
}

/**
 * Competence that enables foraging ants to carry food when it find it.
 */
case class PickFood() extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit = {
    environment.tell(TakeFood(MAX_FOOD - info.foodAmount, info.foodPosition.get), insect)
    context become behaviour(info.updateEnergy(ENERGY_PF))
  }

  override def hasPriority(info: ForagingAntInfo): Boolean = info.foodIsNear && info.foodAmount < MAX_FOOD
}

/**
 * A foraging ant leaves the food in the anthill.
 */
case class StoreFoodInAnthill() extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef,
                     info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit = {
    info.anthill.tell(StoreFood(info.foodAmount), insect)
    val data = info.freeFood().updateEnergy(ENERGY_SF)
    environment.tell(UpdateInsect(data), insect)
    context become behaviour(data)
  }

  override def hasPriority(info: ForagingAntInfo): Boolean = info.isInsideTheAnthill && info.foodAmount > 0
}

/**
 * Competence that enable a foraging ant to follow the traces of the (food) pheromone.
 */
case class FoodPheromoneTaxis() extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit = {
    val delta = info.foodPheromones.toStream.filter(p => p.position --> info.position < ANT_PHEROMONE_THRESHOLD).weightedSum(info.position)
    val data = info.updateEnergy(ENERGY_FPT)
    val newDelta = OrientedVector2DWithNoise(delta./\, MAX_VELOCITY, NOISE) >> (data.inertia * 2)
    val newDelta2 = OrientedVector2D(newDelta./\, MAX_VELOCITY)
    environment.tell(Move(data.position, newDelta2), insect)
    context become behaviour(data.asInstanceOf[ForagingAntInfo].updateFoodPheromones(Seq.empty)) //TODO: should be correct also data without update
  }

  override def hasPriority(info: ForagingAntInfo): Boolean =
    info.foodPheromones.toStream.exists(p => p.position --> info.position < ANT_PHEROMONE_THRESHOLD)
}

/**
 * A foraging ant drops food pheromones when going back to the anthill while carrying food
 */
case class DropFoodPheromone() extends ForagingAntCompetences {

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: ForagingAntInfo, behaviour: ForagingAntInfo => Receive): Unit = {
    environment.tell(AddFoodPheromone(FoodPheromone(info.position, DELTA, info.energy), FOOD_PHEROMONE_THRESHOLD), insect)
    val data = info.updateEnergy(ENERGY_RW)
    environment.tell(UpdateInsect(data), insect)
    context become behaviour(data)
  }

  override def hasPriority(info: ForagingAntInfo): Boolean =
    info.foodAmount > 0 && Random.nextDouble() < Math.pow(info.energy / MAX_ENERGY, 2)
}