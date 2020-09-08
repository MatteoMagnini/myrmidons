package model

import akka.actor.{Actor, ActorLogging}
import utility.Messages.{Clock, MoveMessage}
import model.Environment.EnvironmentState
import TupleOp._

class Environment(state: EnvironmentState) extends Actor with ActorLogging {

  override def receive: Receive = {

    case Clock(value: Int) => ???

    case MoveMessage(pos: (Double, Double), dir: (Int, Int)) =>
      if (isInside(pos >> dir)) { /* send message to ant */}
  }

  private def isInside(pos: (Double, Double)): Boolean = {
    val x = pos.x
    val y = pos.y
    (x > state.boundary.v1.x) && (y > state.boundary.v1.y) &&
    (x < state.boundary.v2.x) && (y > state.boundary.v2.y) &&
    (x > state.boundary.v3.x) && (y < state.boundary.v3.y) &&
    (x < state.boundary.v4.x) && (y < state.boundary.v4.y)
  }

}

object Environment {

  case class Boundary(v1:(Double, Double), v2:(Double, Double), v3:(Double, Double), v4:(Double, Double))

  case class EnvironmentState(boundary: Boundary) {

  }
}