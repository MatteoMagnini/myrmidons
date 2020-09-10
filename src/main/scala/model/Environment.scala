package model
import utility.Messages._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import utility.Messages.{Clock, Move, StartSimulation, UpdateInsect}
import model.insects.{ForagingAnt, ForagingAntInfo, InsectInfo}
import utility.Geometry._

class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, obstacles: Seq[Obstacle], centerSpawn: Boolean) =>
      val ants = if (!centerSpawn) createAntFromDefPosition(nAnts) else createAntFromCenter(nAnts)
      ants.foreach(_ ! Clock(0))
      context.become(defaultBehaviour(state.insertAnts(ants).insertObstacles(obstacles)))

    case Clock(value: Int) if sender == state.gui => state.ants.foreach(_ ! Clock(value))

    case Clock(value: Int) =>
      if (state.antCounter < state.ants.size) context.become(defaultBehaviour(state.incAntCounter()))
      else state.gui ! Clock(value); context.become(defaultBehaviour(state.resetAntCounter()))

    case Move(pos: Vector2D, delta: Vector2D) =>
      import utility.Geometry.TupleOp._
      val newPosition = pos >> delta
      if (state.boundary.hasInside(newPosition) && state.obstacles.forall(! _.isInside(newPosition))) {
        sender ! NewPosition(newPosition, newPosition - pos)
      }

    case UpdateInsect(info: InsectInfo) => state.gui ! UpdateInsect(info)
  }

  private def createAntFromDefPosition(nAnts: Int): Seq[ActorRef] =
    (0 until nAnts).map(i =>
      context.actorOf(ForagingAnt(ForagingAntInfo(), self), s"ant-$i"))

  private def createAntFromCenter(nAnts: Int): Seq[ActorRef] =
    (0 until nAnts).map(i => {
      val randomPosition = state.boundary.center
      context.actorOf(ForagingAnt(ForagingAntInfo(position = randomPosition), self), s"ant-$i")
    })
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
