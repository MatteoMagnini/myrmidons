package model.insects

import akka.actor.{Actor, ActorLogging, ActorRef}
import model.insects.competences.InsectCompetences
import model.insects.info.SpecificInsectInfo

/** An insect is an entity with its own behaviour, it has its own control flow and is reactive to inputs.
 *
 * @tparam A the type of the insect
 */

trait Insect[A <: SpecificInsectInfo[A]] extends Actor with ActorLogging {

  def info: SpecificInsectInfo[A]

  def environment: ActorRef

  /** Use of the subsumption architecture to model the final emerging behaviour by selecting one competence per clock.
   *
   * @param info the insect's state
   * @param competences a set of competences that the ant is able to perform
   * @return the competence with heist priority.
   */
  def subsumption(info: A, competences: Iterable[InsectCompetences[A]]): InsectCompetences[A] =
    competences.filter(c => c.hasPriority(info)).head

}
