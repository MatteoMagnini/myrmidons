package model.insects

import akka.actor.{ActorRef, Props}
import model.environment.pheromones.DangerPheromone
import model.environment.pheromones.DangerPheromoneInfo._
import model.insects.Ants.PatrollingAnt._
import model.insects.competences._
import model.insects.info.PatrollingAntInfo
import utility.Messages._
import utility.RichActor._

case class PatrollingAnt (override val info: PatrollingAntInfo,
                     override val environment: ActorRef) extends Insect[PatrollingAntInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val competences = List(Die[PatrollingAntInfo](),
    GoOutside[PatrollingAntInfo](),
    EatFromTheAnthill[PatrollingAntInfo](),
    GoBackToHome[PatrollingAntInfo](),
    DangerPheromoneTaxis(),
    RandomWalk[PatrollingAntInfo]())

  private val decreasingDangerFunction: Double => Double = x => x - DELTA
  private val dangerIntensity = STARTING_INTENSITY * INTENSITY_FACTOR

  private def defaultBehaviour(data: PatrollingAntInfo): Receive = {

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,competences)(context, environment, self, newData, defaultBehaviour)

    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d)
      if(data.position ~~(p,1E-7) && !data.isInsideTheAnthill) {
        environment ! KillInsect(data)
      } else {
        environment ! UpdateInsect(newData)
      }
      context >>> defaultBehaviour(newData)

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

