package model

import akka.actor.{Actor, ActorLogging}
import utility.Messages.{Clock, MoveMessage, StartSimulation}
import model.Environment.EnvironmentState
import TupleOp._

class Environment(state: EnvironmentState) extends Actor with ActorLogging {

  override def receive: Receive = {

    case StartSimulation(nAnts: Int) => /* Create ants */

    case Clock(value: Int) => /* Send message to ants */

    case MoveMessage(pos: Vector2D, dir: (Int, Int)) =>
      if (state.boundary.isInside(pos >> dir)) {
        /* check obstacles presence and send message to ant and to GUI */
      }
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