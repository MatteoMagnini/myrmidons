package model

import akka.actor.{Actor, ActorLogging}
import utility.Messages.{Clock, MoveMessage, StartSimulation, UpdateInsect}
import model.Environment.EnvironmentState
import TupleOp._
import model.insects.{ForagingAnt, ForagingAntInfo, InsectInfo}

class Environment(state: EnvironmentState) extends Actor with ActorLogging {

  override def receive: Receive = {

    case StartSimulation(nAnts: Int) =>
      val ants = (0 to nAnts).map(i =>
        context.actorOf(ForagingAnt(id = i, ForagingAntInfo(), sender), s"ant-$i"))
      ants.foreach(_ ! Clock(0))

    case Clock(value: Int) => /* Send message to ants */

    case MoveMessage(pos: Vector2D, delta: Vector2D) =>
      if (state.boundary.isInside(pos >> delta)) {
        /* check obstacles presence and send message to ant and to GUI */
      }

    case UpdateInsect(info: InsectInfo) =>
  }
}

object Environment {

  case class Boundary(topLeft: Vector2D, topRight: Vector2D, bottomLeft: Vector2D, bottomRight: Vector2D) {

    def isInside(pos: Vector2D): Boolean = {
      (pos.x >= topLeft.x) && (pos.y >= topLeft.y) &&
      (pos.x <= topRight.x) && (pos.y <= bottomLeft.y)
    }
  }

  case class EnvironmentState(boundary: Boundary) {

  }
}