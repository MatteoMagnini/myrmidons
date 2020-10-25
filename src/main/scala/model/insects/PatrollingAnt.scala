package model.insects

import akka.actor.{ActorRef, Props}
import common.RichActor._
import common.message.AnthillMessage.UpdateAnthillCondition
import common.message.EnvironmentMessage.{NewPosition, Pheromones}
import common.message.InsectMessage.{AddPheromone, EatFood, KillInsect, UpdateInsect}
import common.message.SharedMessage.{Clock, Context}
import common.rTree.ScalaEngine
import model.environment.pheromones.DangerPheromoneInfo._
import model.environment.pheromones.{DangerPheromone, Pheromone}
import model.insects.Ants.PatrollingAnt._
import model.insects.competences._
import model.insects.info.PatrollingAntInfo

/**
 * A patrolling ant with state and behaviour.
 *
 * @param info        of the ant
 * @param environment of the simulation
 */
case class PatrollingAnt(override val info: PatrollingAntInfo,
                         override val environment: ActorRef) extends Ant[PatrollingAntInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val engine = ScalaEngine

  private val competences = List(Die[PatrollingAntInfo](defaultBehaviour),
    GoOutside[PatrollingAntInfo](defaultBehaviour),
    EatFromTheAnthill[PatrollingAntInfo](waitConfirm),
    GoBackToHome[PatrollingAntInfo](defaultBehaviour),
    DangerPheromoneTaxis(defaultBehaviour),
    RandomWalk[PatrollingAntInfo](defaultBehaviour))

  private val decreasingDangerFunction: Double => Double = x => x - DELTA

  private val dangerIntensity = STARTING_INTENSITY * INTENSITY_FACTOR

  private def defaultBehaviour(data: PatrollingAntInfo): Receive = {

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
      context >>> defaultBehaviour(data.updateDangerPheromones(pheromones filterKeys ids.toSet))


    case UpdateAnthillCondition =>
      context >>> defaultBehaviour(data.antEntersAnthill(true))

    case KillInsect(_) =>
      environment ! AddPheromone(DangerPheromone(data.position, decreasingDangerFunction, dangerIntensity),
        DANGER_PHEROMONE_MERGING_THRESHOLD)
      context >>> defaultBehaviour(data.updateEnergy(-MAX_ENERGY))

    case Context(_) => sender ! Context(Some(context))

    case _ => //Do nothing
  }

  private def waitConfirm(data: PatrollingAntInfo): Receive = {

    case EatFood(amount) =>
      val newData = data.updateEnergy(amount * FOOD_ENERGY_CONVERSION)
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    case Context(_) => sender ! Context(Some(context))

    case _ => // Do nothing
  }

  private implicit def pheromonesToFoodPheromones(pheromones: Map[Int, Pheromone]): Seq[DangerPheromone] = {
    pheromones.values.toStream.filter(p => p match {
      case _: DangerPheromone => true
      case _ => false
    }).map(p => p.asInstanceOf[DangerPheromone])
  }
}

object PatrollingAnt {
  def apply(info: PatrollingAntInfo, environment: ActorRef): Props =
    Props(classOf[PatrollingAnt], info, environment)
}

