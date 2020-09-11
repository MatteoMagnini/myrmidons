package model.environment

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import model.insects.{ForagingAnt, ForagingAntInfo, InsectInfo}
import model.Bordered
import utility.Geometry._
import utility.Messages.{Clock, Move, StartSimulation, UpdateInsect, _}

/** Environment actor
  *
  * @param state environment internal state
  */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, obstacles: Seq[Bordered], centerSpawn: Boolean) =>

      val ants = if (!centerSpawn) createAntFromDefPosition(nAnts) else createAntFromCenter(nAnts)
      context.become(defaultBehaviour(EnvironmentInfo(Some(sender), state.boundary, obstacles, ants)))

    case Clock(value: Int) => state.ants.foreach(_ ! Clock(value))

    case Move(pos: Vector2D, delta: Vector2D) =>
      val newPosition = pos >> delta
      if (state.obstacles.forall(_.hasInside(newPosition)) && state.boundary.hasInside(newPosition))
          sender ! NewPosition(newPosition, newPosition - pos)
        else
        /* If ant is moving outside boundary or through an obstacle, invert its new position */
          sender ! NewPosition(pos - delta, delta -)

    case UpdateInsect(info: InsectInfo) =>
      val updatedInfo = state.updateAntsInfo(info)
      /* When all ants return their positions, environment send them to GUI */
      if (updatedInfo.antsInfo.size == state.ants.size) {
        state.gui.get ! RepaintInsects(updatedInfo.antsInfo)
        context.become(defaultBehaviour(state.emptyAntsInfo()))
      } else context.become(defaultBehaviour(updatedInfo))

  }

  /** Returns ants references, created from default position */
  private def createAntFromDefPosition(nAnts: Int): Seq[ActorRef] =
    (0 until nAnts).map(i =>
      context.actorOf(ForagingAnt(ForagingAntInfo(), self), s"ant-$i"))

  /** Returns ants references, created from the center of boundary */
  private def createAntFromCenter(nAnts: Int): Seq[ActorRef] =
    (0 until nAnts).map(i => {
      val randomPosition = state.boundary.center
      context.actorOf(ForagingAnt(ForagingAntInfo(id = i, position = randomPosition), self), s"ant-$i")
    })
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
