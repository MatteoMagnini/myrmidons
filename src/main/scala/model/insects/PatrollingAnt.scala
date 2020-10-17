package model.insects

import akka.actor.{ActorRef, Props}
import model.environment.pheromones.{DangerPheromone, Pheromone}
import model.environment.pheromones.DangerPheromoneInfo._
import model.insects.Ants.PatrollingAnt._
import model.insects.competences._
import model.insects.info.PatrollingAntInfo
import common.Messages._
import common.RichActor._
import common.rTree.RTreeProlog

/**
 * A patrolling ant with state and behaviour.
 *
 * @param info of the ant
 * @param environment of the simulation
 */
case class PatrollingAnt (override val info: PatrollingAntInfo,
                     override val environment: ActorRef) extends Insect[PatrollingAntInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val engine = RTreeProlog()

  private val competences = List(Die[PatrollingAntInfo](defaultBehaviour),
    GoOutside[PatrollingAntInfo](defaultBehaviour),
    EatFromTheAnthill[PatrollingAntInfo](waitConfirm),
    GoBackToHome[PatrollingAntInfo](defaultBehaviour),
    DangerPheromoneTaxis(defaultBehaviour),
    RandomWalk[PatrollingAntInfo](defaultBehaviour))

  private val decreasingDangerFunction: Double => Double = x => x - DELTA

  private val dangerIntensity = STARTING_INTENSITY * INTENSITY_FACTOR

  private def defaultBehaviour(data: PatrollingAntInfo): Receive = {

    /** Execute competence */
    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData, competences)(context, environment, self, newData)

    /** Update position */
    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d)
      if(data.position ~~(p,1E-7) && !data.isInsideTheAnthill) {
        environment ! KillInsect(data)
      } else {
        environment ! UpdateInsect(newData)
      }
      context >>> defaultBehaviour(newData)

    /** Update food pheromones */
    case Pheromones(pheromones, tree) =>
      val ids = engine.query(data.position, tree)
      context >>> defaultBehaviour(data.updateDangerPheromones(pheromones filterKeys ids.toSet))

    /** The ant enters or exits the anthill */
    case UpdateAnthillCondition(value) =>
      context >>> defaultBehaviour(data.updateAnthillCondition(value))

    /** Ant killed in a fight */
    case KillInsect(_) =>
      environment ! AddPheromone(DangerPheromone(data.position, decreasingDangerFunction, dangerIntensity),
        DANGER_PHEROMONE_MERGING_THRESHOLD)
      context >>> defaultBehaviour(data.updateEnergy(-MAX_ENERGY))

    /** Just for tests */
    case Context(_) => sender ! Context(Some(context))

    /** Ignore the rest */
    case _ => //Do nothing
  }

  private def waitConfirm(data: PatrollingAntInfo): Receive = {

    /** Eat food from the environment */
    case EatFood(amount) =>
      val newData = data.updateEnergy(amount * FOOD_ENERGY_CONVERSION)
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    /** Just for tests */
    case Context(_) => sender ! Context(Some(context))

    /** Ignore the rest */
    case _ => // Do nothing
  }

  private implicit def pheromonesToFoodPheromones(pheromones: Map[Int,Pheromone]): Seq[DangerPheromone] = {
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

