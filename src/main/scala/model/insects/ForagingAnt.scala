package model.insects

import akka.actor.{ActorRef, Props}
import model.environment.FoodPheromone
import utility.Geometry.{Vector2D, ZeroVector2D}
import utility.Messages._

/**
  * Ant that performs foraging.
  *
  * @param info        its state.
  * @param environment the environment where it performs actions.
  */
case class ForagingAnt(override val info: ForagingAntInfo,
                       override val environment: ActorRef) extends Insect {

  /**
    * Use of the subsumption architecture to model the final emerging behaviour.
    *
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
    case TakeFood(delta, _) =>
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
      val newData = data.updateEnergy(amount * 10) //TODO: conversion factor from food to energy to be parametrized
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    case x => println("Insect: Should never happen, received message: " + x + " from " + sender)
  }
}

object ForagingAnt {
  def apply(info: InsectInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], info, environment)
}

import ConstantInsectInfo._
case class ForagingAntInfo(override val anthill: ActorRef,
                           override val isInsideTheAnthill: Boolean,
                           override val foodPosition: Option[Vector2D],
                           foodPheromones: Seq[FoodPheromone],
                           override val id: Int,
                           override val position: Vector2D,
                           override val inertia: Vector2D,
                           override val energy: Double,
                           override val time: Int,
                           foodAmount: Double) extends InsectInfo {

  def updateFoodPheromones(pheromones: Seq[FoodPheromone]): InsectInfo =
    this.copy(foodPheromones = pheromones)

  override def updatePosition(newPosition: Vector2D): InsectInfo =
    this.copy(position = newPosition)

  override def updateInertia(newInertia: Vector2D): InsectInfo =
    this.copy(inertia = newInertia)

  override def updateEnergy(delta: Double): InsectInfo =
    this.copy(energy = if (energy + delta > MAX_ENERGY) MAX_ENERGY else energy + delta)

  override def incTime(): InsectInfo =
    this.copy(time = time + 1)

  override def updateAnthillCondition(value: Boolean): InsectInfo =
    this.copy(isInsideTheAnthill = value)

  override def updateFoodPosition(position: Option[Vector2D]): InsectInfo =
    this.copy(foodPosition = position)

  def incFood(amount: Double): ForagingAntInfo =
    this.copy(foodAmount = if (foodAmount + amount > MAX_FOOD) MAX_FOOD else foodAmount + amount)

  def freeFood(): ForagingAntInfo =
    this.copy(foodAmount = STARTING_FOOD_AMOUNT)

}

object ForagingAntInfo {
  def apply(anthill: ActorRef, id: Int = 0, position: Vector2D = STARTING_POSITION, energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): ForagingAntInfo =
    new ForagingAntInfo(anthill, false, None, Seq.empty, id, position, ZeroVector2D(), energy, time, STARTING_FOOD_AMOUNT)
}
