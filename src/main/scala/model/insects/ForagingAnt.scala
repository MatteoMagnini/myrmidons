package model.insects

import akka.actor.{ActorRef, Props}
import model.environment.pheromones.{DangerPheromone, FoodPheromone, Pheromone}
import model.environment.pheromones.DangerPheromoneInfo._
import model.insects.Ants.ForagingAnt._
import model.insects.competences._
import model.insects.info.ForagingAntInfo
import utility.Messages._
import utility.RichActor._
import utility.rTree.RTreeProlog

/**
  * Ant that performs foraging.
  *
  * @param info its state.
  * @param environment the environment where it performs actions.
  */
case class ForagingAnt(override val info: ForagingAntInfo,
                       override val environment: ActorRef) extends Insect[ForagingAntInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val engine = RTreeProlog()
  private val competences = List(Die[ForagingAntInfo](),
    GoOutside[ForagingAntInfo](),
    StoreFoodInAnthill(),
    EatFromTheAnthill[ForagingAntInfo](),
    DropFoodPheromone(),
    CarryFoodToHome(),
    GoBackToHome[ForagingAntInfo](),
    PickFood(),
    FoodPheromoneTaxis(),
    RandomWalk[ForagingAntInfo]())

  private val decreasingDangerFunction: Double => Double = x => x - DELTA
  private val dangerIntensity = STARTING_INTENSITY * INTENSITY_FACTOR

  private def defaultBehaviour(data: ForagingAntInfo): Receive = {

    /* Time is passing */
    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData, competences)(context, environment, self, newData, defaultBehaviour)

    /* The environment confirms the new position */
    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d)
      if(data.position ~~(p,1E-7) && !data.isInsideTheAnthill) {
        environment ! KillInsect(data)
      } else {
        environment ! UpdateInsect(newData)
      }
      context >>> defaultBehaviour(newData)

    /* Update food pheromones */
    case Pheromones(pheromones, tree) =>
      val ids = engine.query(data.position, tree)
      context >>> defaultBehaviour(data.updateFoodPheromones(pheromones filterKeys ids.toSet))

    /* The ant perceive food in its proximity */
    case FoodNear(position) =>
      val newData = data.updateFoodPosition(Some(position))
      context >>> defaultBehaviour(newData)

    /* The ant enters or exits the anthill */
    case UpdateAnthillCondition(value) =>
      context >>> defaultBehaviour(data.updateAnthillCondition(value))

    /* Take food from a food source in the environment */
    case TakeFood(delta, _) =>
      val newData = data.incFood(delta).updateFoodPosition(None)
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    /* Eat food from the environment */
    case EatFood(amount) =>
      val newData = data.updateEnergy(amount * FOOD_ENERGY_CONVERSION)
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    /* Ant killed in a fight */
    case KillInsect(_) =>
      environment ! AddPheromone(DangerPheromone(data.position, decreasingDangerFunction, dangerIntensity),
        DANGER_PHEROMONE_MERGING_THRESHOLD)
      context >>> defaultBehaviour(data.updateEnergy(-MAX_ENERGY))

    /* Just for tests */
    case Context(_) => sender ! Context(Some(context))

    case x => //System.err.println(s"ForagingAnt ${info.id}: received unhandled message $x from $sender")
  }

  private implicit def pheromonesToFoodPheromones(pheromones: Map[Int,Pheromone]): Seq[FoodPheromone] = {
    pheromones.values.toStream.filter(p => p match {
      case p: FoodPheromone => true
      case _ => false
    }).map(p => p.asInstanceOf[FoodPheromone])
  }
}

object ForagingAnt {
  def apply(info: ForagingAntInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], info, environment)
}
