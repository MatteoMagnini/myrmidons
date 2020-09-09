package model

import akka.actor.{Actor, ActorLogging}
import utility.Messages.{Clock, Move, StartSimulation, UpdateInsect}
import model.Environment.EnvironmentState
import model.insects.{ForagingAnt, ForagingAntInfo, InsectInfo}
import utility.Geometry._

class Environment(state: EnvironmentState) extends Actor with ActorLogging {

  override def receive: Receive = {

    case StartSimulation(nAnts: Int) =>
      val ants = (0 to nAnts).map(i =>
        context.actorOf(ForagingAnt(id = i, ForagingAntInfo(), sender), s"ant-$i"))
      ants.foreach(_ ! Clock(0))

    case Clock(value: Int) => /* Send message to ants */

    case Move(pos: Vector, delta: Vector) =>
      if (state.boundary.isInside(pos >> delta)) {
        /* check obstacles presence and send message to ant and to GUI */
      }

    case UpdateInsect(info: InsectInfo) =>
  }
}

object Environment {

  case class Boundary( topLeft: Vector, topRight: Vector, bottomLeft: Vector, bottomRight: Vector) {

    def isInside(pos: Vector): Boolean = {
      (pos.x >= topLeft.x) && (pos.y >= topLeft.y) &&
      (pos.x <= topRight.x) && (pos.y <= bottomLeft.y)
    }
  }

  case class EnvironmentState(boundary: Boundary) {

  }
}