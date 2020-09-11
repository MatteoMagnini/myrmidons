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
case class EnvironmentInfo(gui: Option[ActorRef], boundary: Boundary, obstacles: Seq[Bordered],
                           ants: Seq[ActorRef], antsInfo: Seq[InsectInfo]) {

  /** Returns info, adding ant information */
  def updateAntsInfo(antInfo: InsectInfo): EnvironmentInfo = this.copy(antsInfo = antInfo +: antsInfo)

  /** Returns info, emptying ants information */
  def emptyAntsInfo(): EnvironmentInfo = this.copy(antsInfo = Seq.empty)

}

object EnvironmentInfo {
  def apply(boundary: Boundary): EnvironmentInfo =
    new EnvironmentInfo(None, boundary, Seq.empty, Seq.empty, Seq.empty)

  def apply(gui: Option[ActorRef], boundary: Boundary, obstacles:Seq[Bordered], ants: Seq[ActorRef] ): EnvironmentInfo =
    new EnvironmentInfo(gui, boundary, obstacles, ants, Seq.empty)
}
