package model.insects

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import utility.Geometry._
import utility.Messages.{KillAnt, _}
import ConstantInsectInfo._
import model.environment.FoodPheromone
import model.insects

import scala.util.Random
object Constant {
  val NOISE = 0.3
  val FOOD_PHEROMONE_THRESHOLD: Double = 10.0
  val MAX_VELOCITY: Double = 5
  val MIN_VELOCITY: Double = - 5
  val INERTIA_FACTOR: Double = 0.9
  val FOOD_EATEN_PER_STEP: Double = 0.5
  val ENERGY_RW: Double = - 0.3
  val ENERGY_PF: Double = -0.2
  val ENERGY_EATING: Double = - 0.1
  val ENERGY_SF: Double = -0.1
  val ENERGY_FPT: Double = - 1.5
  val RANDOM: Random.type = scala.util.Random
}

import Constant._
import utility.PheromoneSeq._

/** A competence is the minimal building block to achieve a more complex behaviour. */
trait Competence {

  def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit

  /**
   * Check if this competence may be executed.
   * @param info the insect's state.
   * @return true if this competence has priority over the all its less important competences.
   */
  def hasPriority(info: InsectInfo): Boolean

}

/** Competence performing a random walk. */
object RandomWalk extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {

    val data = info.updateEnergy(ENERGY_RW)
    val delta: Vector2D = RandomVector2D(MIN_VELOCITY, MAX_VELOCITY, info.inertia * INERTIA_FACTOR)
    environment.tell(Move(data.position, delta),ant)
    context become behaviour(data)
  }

  override def hasPriority(info: InsectInfo): Boolean = true
}

/** Competence forcing an ant to go back to the anthill when its energy is low. */
object GoBackToHome extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive ): Unit = {
    val data = info.updateEnergy(ENERGY_RW)
    info.anthill.tell(AntTowardsAnthill(info.position, MAX_VELOCITY, NOISE, info.isInsideTheAnthill),ant)
    context become behaviour(data)
  }

  override def hasPriority( info: InsectInfo ): Boolean = info match {
    case i: ForagingAntInfo => i.foodAmount > 0 || i.energy < 40
    case x => x.energy < 40 //TODO: clearly to be parametrized
  }
}

/**
  * Competence forcing an insect to exit the anthill when its energy level is high.
  */
object GoOutside extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive ): Unit = {
    val data = info.updateEnergy(ENERGY_RW).updateAnthillCondition(false)
    val delta: Vector2D = RandomVector2D(MIN_VELOCITY, MAX_VELOCITY, info.inertia * INERTIA_FACTOR)
    environment.tell(Move(data.position, delta),ant)
    context become behaviour(data)
  }

  override def hasPriority( info: InsectInfo ): Boolean = info match {
    case i: insects.ForagingAntInfo => i.isInsideTheAnthill && i.energy > 80 && i.foodAmount == 0//TODO: clearly to be parametrized
    case x => x.isInsideTheAnthill && x.energy > 80
  }
}

/**
 * Try to eat food from the anthill when the insect is inside it.
 */
object EatFromTheAnthill extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    info.anthill.tell(EatFood(FOOD_EATEN_PER_STEP),ant)
    val data = info.updateEnergy(ENERGY_EATING).updateInertia(ZeroVector2D())
    context become behaviour(data)
  }

  override def hasPriority(info: InsectInfo): Boolean = info.isInsideTheAnthill
}

/**
 * Competence that enables ant to carry food when it find it.
 */
object PickFood extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    println(s"Ant ${info.id} picking food")
    info match {
      case i: ForagingAntInfo if i.foodIsNear =>
        environment.tell(TakeFood(MAX_FOOD - i.foodAmount, i.foodPosition.get), ant)
      case _ => println("Only a foraging ant can pick up food.")
    }
    context become behaviour(info.updateEnergy(ENERGY_PF))
  }

  override def hasPriority(info: InsectInfo): Boolean = info match {
    case i: ForagingAntInfo => i.foodIsNear && i.foodAmount < MAX_FOOD
    case _ => false
  }
}

/**
 * An ant leaves the food in the anthill.
 */
object StoreFoodInAnthill extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    println(s"Ant ${info.id} storing food in anthill")
    val data = info match {
      case i: ForagingAntInfo =>
        i.anthill.tell(StoreFood(i.foodAmount),ant)
        i.freeFood().updateEnergy(ENERGY_SF)

      case x => x.updateEnergy(ENERGY_SF)
    }
    environment.tell(UpdateInsect(data), ant)
    context become behaviour(data)
  }


  override def hasPriority(info: InsectInfo): Boolean = info match {
    case i: ForagingAntInfo => i.isInsideTheAnthill && i.foodAmount > 0
    case _ => false
  }
}

/**
 * When energy is 0 the insect dies. Must be the first competence for every insects.
 */
object Die extends Competence {
  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    environment.tell(KillAnt(info.id), ant)
  }

  override def hasPriority(info: InsectInfo): Boolean = info.energy <= 0
}

/**
 *  Competence that enable an ant to follow the traces of the (food) pheromone.
 */
object FoodPheromoneTaxis extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    val delta = info match {
      case i: ForagingAntInfo =>
        i.foodPheromones.toStream.filter(p => p.position --> i.position < FOOD_PHEROMONE_THRESHOLD ).weightedSum(i.position)
      case _ => println("Only a foraging ant can do FoodPheromoneTaxis"); ZeroVector2D()
    }
    val data = info.updateEnergy(ENERGY_FPT)
    environment.tell(Move(data.position, OrientedVector2DWithNoise(delta./\, MAX_VELOCITY, NOISE)),ant)
    context become behaviour(data.asInstanceOf[ForagingAntInfo].updateFoodPheromones(Seq.empty)) //TODO: should be correct also data without update
  }

  override def hasPriority(info: InsectInfo): Boolean = info match {
    case i: ForagingAntInfo => i.foodPheromones.toStream.exists(p => p.position --> i.position < FOOD_PHEROMONE_THRESHOLD)
    case _ => false
  }
}

import model.environment.FoodPheromoneInfo._

object DropFoodPheromone extends Competence {

  override def apply(context: ActorContext, environment: ActorRef, ant: ActorRef, info: InsectInfo, behaviour: InsectInfo => Receive): Unit = {
    environment.tell(AddFoodPheromones(FoodPheromone(info.position, DELTA, info.energy * INTENSITY_FACTOR)),ant)
    val data = info.updateEnergy(ENERGY_RW)
    environment.tell(UpdateInsect(data), ant)
    context become behaviour(data)
  }

  override def hasPriority(info: InsectInfo): Boolean = info match {
    case i: ForagingAntInfo => i.foodAmount > 0 && Random.nextDouble() < Math.pow(i.energy/MAX_ENERGY,2)
    case _ => false
  }
}
