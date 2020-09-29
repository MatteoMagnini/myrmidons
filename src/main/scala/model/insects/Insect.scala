package model.insects

import akka.actor.{Actor, ActorLogging, ActorRef}
import model.insects.info.SpecificInsectInfo

/**
  * An insect is an entity with its own behaviour.
  * For this reason it extends Actor, it has its own control flow and is reactive to inputs (messages).
  * It also holds the information (state) of the insect.
  */

trait Insect[A <: SpecificInsectInfo[A]] extends Actor with ActorLogging {

  def info: SpecificInsectInfo[A]

  def environment: ActorRef

  /**
   * Use of the subsumption architecture to model the final emerging behaviour by selecting one competence per clock.
   *
   * @param competences a set of competences that the ant is able to perform.
   * @return the competence with heist priority.
   */
  def subsumption(data: A, competences: Competence[A]*): Competence[A] =
    competences.filter(c => c.hasPriority(data)).head
}
