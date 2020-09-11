package model.environment

import akka.actor.ActorRef
import model.Bordered
import model.insects.InsectInfo

/** Internal state of environment.
  *
  * @param gui reference to gui actor
  * @param boundary boundary constrains of environment
  * @param obstacles obstacles in environment
  * @param ants references to ant actors
  * @param antsInfo ants information
  */
case class EnvironmentInfo(gui: ActorRef, boundary: Boundary, obstacles: Seq[Bordered],
                           ants: Seq[ActorRef], antsInfo: Seq[InsectInfo]) {

  /** Returns info, adding ants references */
  def insertAnts(ants: Seq[ActorRef]): EnvironmentInfo = this.copy(ants = ants)

  /** Returns info, adding obstacles */
  def insertObstacles(obstacles: Seq[Bordered]): EnvironmentInfo = this.copy(obstacles = obstacles)

  /** Returns info, adding ant information */
  def updateAntsInfo(antInfo: InsectInfo): EnvironmentInfo = this.copy(antsInfo = antInfo +: antsInfo)

  /** Returns info, emptying ants information */
  def emptyAntsInfo(): EnvironmentInfo = this.copy(antsInfo = Seq.empty)

}

object EnvironmentInfo {
  def apply(gui: ActorRef, boundary: Boundary): EnvironmentInfo =
    new EnvironmentInfo(gui, boundary, Seq.empty, Seq.empty, Seq.empty)
}
