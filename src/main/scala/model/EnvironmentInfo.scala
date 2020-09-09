package model

import akka.actor.ActorRef


case class EnvironmentInfo(gui: ActorRef, boundary: Boundary, obstacles: Seq[Obstacle], ants: Seq[ActorRef], antCounter: Int) {

  def insertAnts(ants: Seq[ActorRef]): EnvironmentInfo = this.copy(ants = ants)

  def insertObstacles(obstacles: Seq[Obstacle]): EnvironmentInfo = this.copy(obstacles = obstacles)

  def incAntCounter(): EnvironmentInfo = this.copy(antCounter = antCounter + 1)

  def resetAntCounter(): EnvironmentInfo = this.copy(antCounter = 0)

}

object EnvironmentInfo {

  def apply(gui: ActorRef, boundary: Boundary): EnvironmentInfo = new EnvironmentInfo(gui, boundary, Seq.empty, Seq.empty, 0)

}
