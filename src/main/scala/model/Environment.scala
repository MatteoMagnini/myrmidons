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
      context.become(defaultBehaviour(state.insertAnts(ants).insertObstacles(obstacles)))

    case Clock(value: Int) => state.ants.foreach(_ ! Clock(value));  println("Env. Logic time: " +value )

    case Move(pos: Vector2D, delta: Vector2D) =>
      import utility.Geometry.TupleOp._
      val newPosition = pos >> delta
      if ( state.obstacles.forall(! _.isInside(newPosition))) {
        sender ! NewPosition(newPosition, newPosition - pos)
      }
      else if(state.boundary.hasInside(newPosition)){
        sender ! NewPosition(pos - delta, delta-)
      }

    case UpdateInsect(info: InsectInfo) => state.gui ! UpdateInsect(info); println("Env 2 Time: "+ info.time)
  }

  private def createAntFromDefPosition(nAnts: Int): Seq[ActorRef] =
    (0 until nAnts).map(i =>
      context.actorOf(ForagingAnt(ForagingAntInfo(), self), s"ant-$i"))

  private def createAntFromCenter(nAnts: Int): Seq[ActorRef] =
    (0 until nAnts).map(i => {
      val randomPosition = state.boundary.center
      context.actorOf(ForagingAnt(ForagingAntInfo(id = i, position = randomPosition), self), s"ant-$i")
    })
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
