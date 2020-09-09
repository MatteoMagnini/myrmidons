package model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import utility.Messages._
import model.insects.{ForagingAnt, ForagingAntInfo, InsectInfo}

class Environment(state: EnvironmentState) extends Actor with ActorLogging {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: EnvironmentState): Receive = {

    case StartSimulation(nAnts: Int) =>
      log.debug("Started!")
      val ants = (0 to nAnts).map(i =>
        context.actorOf(ForagingAnt(id = i, ForagingAntInfo(), sender), s"ant-$i"))
      ants.foreach(_ ! Clock(1))

    case Clock(value: Int) => state.ants.foreach(_ ! Clock(value))

    case MoveMessage(pos: Vector2D, delta: Vector2D) =>
      val newPosition = pos >> delta
      if (state.boundary.hasInside(newPosition)) {
        /* check obstacles presence*/
        sender ! NewPosition(newPosition)
      }

    case UpdateInsect(info: InsectInfo) => state.gui ! UpdateInsect(info)
  }
}

object Environment {
  def apply(state: EnvironmentState): Props = Props(classOf[Environment], state)
}


case class EnvironmentState(gui: ActorRef, boundary: Boundary, ants: Seq[ActorRef])

object EnvironmentState {

  def apply(gui: ActorRef, boundary: Boundary, ants: Seq[ActorRef]): EnvironmentState = new EnvironmentState(gui, boundary, ants)
  def apply(gui: ActorRef, boundary: Boundary): EnvironmentState = new EnvironmentState(gui, boundary, Seq.empty)
}

