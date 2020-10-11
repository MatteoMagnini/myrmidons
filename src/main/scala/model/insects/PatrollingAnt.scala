package model.insects

import akka.actor.{ActorRef, Props}
import model.environment.pheromones.DangerPheromone
import model.insects.competences.{DangerPheromoneTaxis, Die, EatFromTheAnthill, GoBackToHome, GoOutside, RandomWalk}
import model.insects.info.PatrollingAntInfo
import utility.Messages.{AddDangerPheromone, Clock, Context, DangerPheromones, EatFood, KillInsect, NewPosition, UpdateAnthillCondition, UpdateInsect}
import utility.Parameters.Insects.Ants.ForagingAnt._

case class PatrollingAnt (override val info: PatrollingAntInfo,
                     override val environment: ActorRef) extends Insect[PatrollingAntInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val competences = List(Die[PatrollingAntInfo](),
    GoOutside[PatrollingAntInfo](),
    EatFromTheAnthill[PatrollingAntInfo](),
    GoBackToHome[PatrollingAntInfo](),
    DangerPheromoneTaxis(),
    RandomWalk[PatrollingAntInfo]())

  import utility.Parameters.Pheromones.DangerPheromoneInfo._
  private val decreasingDangerFunction: Double => Double = x => x - DELTA
  private val dangerIntensity = STARTING_INTENSITY * INTENSITY_FACTOR

  private def defaultBehaviour(data: PatrollingAntInfo): Receive = {

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,competences)(context, environment, self, newData, defaultBehaviour)

    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d)
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    /**
     * Update food pheromones.
     */
    case DangerPheromones(pheromones) =>
      context >>> defaultBehaviour(data.updateDangerPheromones(pheromones))

    /**
     * The ant enters or exits the anthill.
     */
    case UpdateAnthillCondition(value) =>
      context >>> defaultBehaviour(data.updateAnthillCondition(value))

    /**
     * Eat food from the environment.
     */
    case EatFood(amount) =>
      val newData = data.updateEnergy(amount * FOOD_ENERGY_CONVERSION)
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    /**
     * Ant killed in a fight
     */
    case KillInsect(_) =>
      environment ! AddDangerPheromone(DangerPheromone(data.position, decreasingDangerFunction, dangerIntensity),DANGER_PHEROMONE_MERGING_THRESHOLD)
      context >>> defaultBehaviour(data.updateEnergy(-MAX_ENERGY))

    /**
     * Just for tests
     */
    case Context(_) => sender ! Context(Some(context))

    case x => //Discarding useless messages
  }
}

object PatrollingAnt {
  def apply(info: PatrollingAntInfo, environment: ActorRef): Props =
    Props(classOf[PatrollingAnt], info, environment)
}

