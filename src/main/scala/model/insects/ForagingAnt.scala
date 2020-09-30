package model.insects

import akka.actor.{ActorRef, Props}
import model.insects.competences.{CarryFoodToHome, Die, DropFoodPheromone, EatFromTheAnthill, FoodPheromoneTaxis, GoBackToHome, GoOutside, PickFood, RandomWalk, StoreFoodInAnthill}
import model.insects.info.ForagingAntInfo
import utility.Messages._
import utility.Parameters.ForagingAnt._

/**
  * Ant that performs foraging.
  *
  * @param info its state.
  * @param environment the environment where it performs actions.
  */
case class ForagingAnt(override val info: ForagingAntInfo,
                       override val environment: ActorRef) extends Insect[ForagingAntInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private def defaultBehaviour(data: ForagingAntInfo): Receive = {

    /**
      * Time is passing.
      */
    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,
        Die[ForagingAntInfo](),
        GoOutside[ForagingAntInfo](),
        StoreFoodInAnthill(),
        EatFromTheAnthill[ForagingAntInfo](),
        DropFoodPheromone(),
        CarryFoodToHome(),
        GoBackToHome[ForagingAntInfo](),
        PickFood(),
        FoodPheromoneTaxis(),
        RandomWalk[ForagingAntInfo]())(context, environment, self, newData, defaultBehaviour)

    /**
      * The environment confirms the new position.
      */
    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d)
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    /**
      * Update food pheromones.
      */
    case FoodPheromones(pheromones) => data match {
      case f: ForagingAntInfo => context become defaultBehaviour(f.updateFoodPheromones(pheromones))
      case _ => System.err.println(s"ForagingAnt ${info.id}: general error while receiving FoodPheromones message (should never happen)")
    }

    /**
      * The ant perceive food in its proximity.
      */
    case FoodNear(position) =>
      val newData = data.updateFoodPosition(Some(position))
      context >>> defaultBehaviour(newData)

    /**
      * The ant enters or exits the anthill.
      */
    case UpdateAnthillCondition(value) =>
      context >>> defaultBehaviour(data.updateAnthillCondition(value))

    /**
      * Take food from a food source in the environment.
      */
    case TakeFood(delta, _) =>
      val newData = data match {
        case d: ForagingAntInfo => d.incFood(delta).updateFoodPosition(None)
        case x => x
      }
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    /**
      * Eat food from the environment.
      */
    case EatFood(amount) =>
      val newData = data.updateEnergy(amount * FOOD_ENERGY_CONVERSION)
      environment ! UpdateInsect(newData)
      context >>> defaultBehaviour(newData)

    case Context(_) => sender ! Context(Some(context))

    case x => System.err.println(s"ForagingAnt ${info.id}: received unhandled message $x from $sender")
  }
}

object ForagingAnt {
  def apply(info: ForagingAntInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], info, environment)
}
