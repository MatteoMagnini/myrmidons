package model.insects

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import utility.Messages._
import utility.PheromoneSeq._

/**
 * An insect is an entity with its own behaviour.
 * For this reason it extends Actor, it has its own control flow and is reactive to inputs (messages).
 * It also holds the information (state) of the insect.
 */

trait Insect extends Actor with ActorLogging {

  def info: InsectInfo
  def environment: ActorRef
}

/**
 * Ant that performs foraging.
 * @param info its state.
 * @param environment the environment where it performs actions.
 */
case class ForagingAnt(override val info: ForagingAntInfo,
                       override val environment: ActorRef) extends Insect {

  /**
   * Use of the subsumption architecture to model the final emerging behaviour.
   * @param competences a set of competences that the ant is able to perform.
   * @return the competence with heist priority.
   */
  private def subsumption(data: InsectInfo, competences: Competence*): Competence = competences.filter(c => c.hasPriority(data)).take(1).last

  override def receive: Receive = defaultBehaviour(info)

  private def defaultBehaviour(data: InsectInfo): Receive = {

    /**
     * Time is passing.
     */
    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,
        Die,
        GoOutside,
        StoreFoodInAnthill,
        EatFromTheAnthill,
        DropFoodPheromone,
        GoBackToHome,
        PickFood,
        FoodPheromoneTaxis,
        RandomWalk)(context, environment, self, newData, defaultBehaviour)

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
      case _ => System.err.println("Creation of foraging ant with wrong insect information")
    }

    /**
     * The ant perceive food in its proximity.
     */
    case FoodNear(position) =>
      val newData = data.updateFoodPosition(Some(position))
      //environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    /**
     * The ant enters or exits the anthill.
     */
    case UpdateAnthillCondition(value) =>
      context become defaultBehaviour(data.updateAnthillCondition(value))

    /**
     * Take food from a food source in the environment.
     */
    case TakeFood(delta,_) =>
      val newData = data match {
        case d: ForagingAntInfo => d.incFood(delta).updateFoodPosition(None)
        case x => x
      }
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    /**
     * Eat food from the environment.
     */
    case EatFood(amount) =>
      val newData = data.updateEnergy(amount*10) //TODO: conversion factor from food to energy to be parametrized
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    case x => println("Should never happen, received message: " + x + " from " + sender)
  }
}

object ForagingAnt {
  def apply(info: InsectInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], info, environment)
}