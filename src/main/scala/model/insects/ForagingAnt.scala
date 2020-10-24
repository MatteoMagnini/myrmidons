package model.insects

import akka.actor.{ActorRef, Props}
import common.RichActor._
import common.message.AnthillMessage.UpdateAnthillCondition
import common.message.EnvironmentMessage.{FoodNear, NewPosition, Pheromones}
import common.message.InsectMessage._
import common.message.SharedMessage.{Clock, Context}
import common.rTree.{ScalaEngine, tree}
import model.environment.pheromones.DangerPheromoneInfo._
import model.environment.pheromones.{DangerPheromone, FoodPheromone, Pheromone}
import model.insects.Ants.ForagingAnt._
import model.insects.competences._
import model.insects.info.ForagingAntInfo

/**
 * Ant that performs foraging.
 *
 * @param info        its state.
 * @param environment the environment where it performs actions.
 */
case class ForagingAnt(override val info: ForagingAntInfo,
                       override val environment: ActorRef) extends Ant[ForagingAntInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val engine = ScalaEngine

  private val competences = List(Die[ForagingAntInfo](defaultBehaviour),
    GoOutside[ForagingAntInfo](defaultBehaviour),
    StoreFoodInAnthill(defaultBehaviour),
    EatFromTheAnthill[ForagingAntInfo](waitConfirm),
    DropFoodPheromone(defaultBehaviour),
    CarryFoodToHome(defaultBehaviour),
    GoBackToHome[ForagingAntInfo](defaultBehaviour),
    PickFood(waitConfirm),
    FoodPheromoneTaxis(defaultBehaviour),
    RandomWalk[ForagingAntInfo](defaultBehaviour))

  private val decreasingDangerFunction: Double => Double = x => x - DELTA
  private val dangerIntensity = STARTING_INTENSITY * INTENSITY_FACTOR

  private def defaultBehaviour(data: ForagingAntInfo): Receive = {

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incrementTime()
      subsumption(newData, competences)(context, environment, self, newData)

    case NewPosition(position, delta) =>
      val newData = data.updatePosition(position).updateInertia(delta)
      if (data.position ~~ (position, 1E-7) && !data.isInsideTheAnthill) {
        environment ! KillInsect(data)
      } else {
        environment ! UpdateInsect(newData)
      }
      context >>> defaultBehaviour(newData)

    case Pheromones(pheromones, tree) =>
      val ids = engine.query(data.position, tree)
      context >>> defaultBehaviour(data.updateFoodPheromones(pheromones filterKeys ids.toSet))

    case FoodNear(position) =>
      val newData = data.updateFoodPosition(Some(position))
      context >>> defaultBehaviour(newData)

    case UpdateAnthillCondition =>
      context >>> defaultBehaviour(data.antEntersAnthill(true))

    case KillInsect(_) =>
      environment ! AddPheromone(DangerPheromone(data.position, decreasingDangerFunction, dangerIntensity),
        DANGER_PHEROMONE_MERGING_THRESHOLD)
      context >>> defaultBehaviour(data.updateEnergy(-MAX_ENERGY))

    case Context(_) => sender ! Context(Some(context))

    case _ => // Do nothing
  }

  private def waitConfirm(data: ForagingAntInfo): Receive = {

    case TakeFood(delta, _) =>
      val newData = data.incFood(delta).updateFoodPosition(None)
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    case EatFood(amount) =>
      val newData = data.updateEnergy(amount * FOOD_ENERGY_CONVERSION)
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    case Context(_) => sender ! Context(Some(context))

    case _ => // Do nothing
  }

  private implicit def pheromonesToFoodPheromones(pheromones: Map[Int, Pheromone]): Seq[FoodPheromone] = {
    pheromones.values.toStream.filter(p => p match {
      case _: FoodPheromone => true
      case _ => false
    }).map(p => p.asInstanceOf[FoodPheromone])
  }
}

object ForagingAnt {
  def apply(info: ForagingAntInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], info, environment)
}
