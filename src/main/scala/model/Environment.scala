package model
import utility.Messages._
import akka.actor.{Actor, ActorLogging, Props}
import utility.Messages.{Clock, Move, StartSimulation, UpdateInsect}
import model.insects.{ForagingAnt, ForagingAntInfo, InsectInfo}
import utility.Geometry._

class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, obstacles: Seq[Obstacle]) =>
      log.debug("Started!")
      val ants = (0 until nAnts).map(i =>
        context.actorOf(ForagingAnt(id = i, ForagingAntInfo(), self), s"ant-$i"))
      context.become(defaultBehaviour(state.insertAnts(ants)))

    case Clock(value: Int) if sender == state.gui => state.ants.foreach(_ ! Clock(value)); log.debug("CLOCK " + state.ants.size)

    //TODO
   // case Clock(value: Int) => /* increment ant counter */ state.gui ! Clock(value)

    case Move(pos: Vector2D, delta: Vector2D) =>
      log.debug("Move")
      val newPosition = pos >> delta
     // if (state.boundary.hasInside(newPosition)) {
        /* check obstacles presence*/
        sender ! NewPosition(newPosition)
     // }

    case UpdateInsect(info: InsectInfo) => log.debug(" "+state.gui); state.gui ! UpdateInsect(info)
  }
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
