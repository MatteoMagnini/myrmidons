package model.environment

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import model.insects.{ForagingAnt, ForagingAntInfo, InsectInfo}
import utility.Geometry._
import utility.Messages.{Clock, Move, StartSimulation, UpdateInsect, _}
import model.BorderedEntityFactory._
import model.Food
import model.anthill.{Anthill, AnthillInfo}

/** Environment actor
  *
  * @param state environment internal state
  */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, centerSpawn: Boolean, obstaclesPresence, foodPresence) =>
      val anthill = context.actorOf(Anthill(AnthillInfo(state.boundary.center),self), name = "anthill")
      val ants = if (!centerSpawn) createAntFromDefPosition(nAnts,anthill) else createAntFromCenter(nAnts,anthill)
      val obstacles = if(obstaclesPresence)(0 to 3).map(_ =>
        createRandomSimpleObstacle(state.boundary.topLeft.x, state.boundary.bottomRight.x, 40, 40)) else Seq.empty
      val foods = if(foodPresence)(0 to 3).map(_ =>
        createRandomFood(state.boundary.topLeft.x, state.boundary.bottomRight.x,1000)) else Seq.empty
      sender ! Repaint(foods ++ obstacles)
      context.become(defaultBehaviour(EnvironmentInfo(Some(sender), state.boundary, foods ++ obstacles, ants, Some(anthill))))

    case Clock(value: Int) => state.ants.foreach(_ ! Clock(value))

    case Move(pos: Vector2D, delta: Vector2D) =>
      val newPosition = pos >> delta
      if(state.boundary.hasInside(newPosition)) {
        if (state.obstacles.forall(!_.hasInside(newPosition))) {
          sender ! NewPosition(newPosition, newPosition - pos)
        } else {
          /* If ant is moving outside boundary or through an obstacle, invert its new position */
          val collision = state.obstacles.find(_.hasInside(newPosition)).get
          collision match {
            case x@Food(_, _) =>
              sender ! Eat
              context become defaultBehaviour(state.updateFood(x, x-10))
            case _ => log.debug("Collision")
              sender ! NewPosition(pos, delta -)
          }
        }
      } else sender ! NewPosition(pos, delta -)

    case UpdateInsect(info: InsectInfo) =>
      val updatedInfo = state.updateAntsInfo(info)
      /* When all ants return their positions, environment send them to GUI */
      if (updatedInfo.antsInfo.size == state.ants.size) {
        state.gui.get ! Repaint(updatedInfo.antsInfo ++ updatedInfo.obstacles)
        context.become(defaultBehaviour(state.emptyAntsInfo()))
      } else context.become(defaultBehaviour(updatedInfo))

  }

  /** Returns ants references, created from default position */
  private def createAntFromDefPosition(nAnts: Int, anthill: ActorRef): Seq[ActorRef] =
    (0 until nAnts).map(i =>
      context.actorOf(ForagingAnt(ForagingAntInfo(anthill, id = i), self), s"ant-$i"))

  /** Returns ants references, created from the center of boundary */
  private def createAntFromCenter(nAnts: Int, anthill: ActorRef): Seq[ActorRef] =
    (0 until nAnts).map(i => {
      val randomPosition = state.boundary.center
      context.actorOf(ForagingAnt(ForagingAntInfo(anthill, id = i, position = randomPosition), self), s"ant-$i")
    })
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
