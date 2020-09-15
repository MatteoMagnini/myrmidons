package model.insects

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import utility.Messages._

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

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,GoBackToHome,FoodPheromoneTaxis,RandomWalk)(context, environment, self, newData, defaultBehaviour)

    case NewPosition(p, d) =>
      val newData = data.updatePosition(p)
      val newData2 = newData.updateInertia(d)
      environment ! UpdateInsect(newData2)
      context become defaultBehaviour(newData2)

    case FoodPheromones(entities) => data match {
      case f: ForagingAntInfo => context become defaultBehaviour(f.addPheromones(entities))
      case _ => System.err.println("Creation of foraging ant with wrong insect information")
    }

    case Eat =>
      val newData = data.updateEnergy(10)
      environment ! UpdateInsect(newData)
      log.debug("Eat")
      context become defaultBehaviour(newData)

    case x => println("Should never happen, received message: " + x.getClass + " from " + sender)

  }
}

object ForagingAnt {
  def apply(info: InsectInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], info, environment)
}