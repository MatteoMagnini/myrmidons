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

    case Clock(value: Int) => state.ants.foreach(_ ! Clock(value))

    case Move(pos: Vector2D, delta: Vector2D) =>
      import utility.Geometry.TupleOp._
      /*if (state.obstacles.forall(! _.isInside(newPosition))) {
        sender ! NewPosition(newPosition, newPosition - pos)
      }
      else*/
      if(!state.boundary.hasInside(pos >> delta)){
        sender ! NewPosition(pos - delta, delta-)
      }else {
        sender ! NewPosition(pos >> delta, pos >> delta - pos)
      }

    case UpdateInsect(info: InsectInfo) =>
      val updateInfo = state.updateAntsInfo(info)
      if(updateInfo.antsInfo.size == state.ants.size){
        state.gui ! RepaintInsects(updateInfo.antsInfo)
        context.become(defaultBehaviour(state.emptyAntsInfo()))
      }else {
        context.become(defaultBehaviour(updateInfo))
      }
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
