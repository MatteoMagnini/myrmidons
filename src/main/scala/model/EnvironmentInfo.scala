package model

import akka.actor.ActorRef


case class EnvironmentInfo(gui: ActorRef, boundary: Boundary, ants: Seq[ActorRef], antCounter: Int) {

  def insertAnts(ants: Seq[ActorRef]): EnvironmentInfo = EnvironmentInfo(gui, boundary, ants, antCounter)

  def incAntCounter(): EnvironmentInfo = EnvironmentInfo(gui, boundary, ants, antCounter + 1)

  def resetAntCounter(): EnvironmentInfo = EnvironmentInfo(gui, boundary, ants, 0)

}

object EnvironmentInfo {

  def apply(gui: ActorRef, boundary: Boundary, ants: Seq[ActorRef]): EnvironmentInfo = new EnvironmentInfo(gui, boundary, ants, 0)
  def apply(gui: ActorRef, boundary: Boundary): EnvironmentInfo = new EnvironmentInfo(gui, boundary, Seq.empty, 0)

}
