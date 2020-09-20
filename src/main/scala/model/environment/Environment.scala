package model.environment

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import model.insects.{ConstantInsectInfo, ForagingAnt, ForagingAntInfo, InsectInfo}
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

      val anthill = context.actorOf(Anthill(AnthillInfo(state.boundary.center, 15, foodAmount = 1000), self), name = "anthill")
      val ants = if (!centerSpawn) createAntFromRandomPosition(nAnts, anthill) else createAntFromCenter(nAnts, anthill)

      val obstacles = if (obstaclesPresence.isDefined) (0 until obstaclesPresence.get).map(_ =>
        createRandomSimpleObstacle(200, 600)) else Seq.empty

      val foods = if (foodPresence.isDefined) (0 until foodPresence.get).map(_ =>
        createRandomFood(state.boundary.topLeft.x, state.boundary.bottomRight.x)) else Seq.empty

      context become defaultBehaviour(EnvironmentInfo(Some(sender), state.boundary, foods ++ obstacles, ants, anthill, state.anthillInfo))

    case Clock(value: Int) =>
      state.ants.values.foreach(_ ! Clock(value))
      state.anthill match {
        case Some(x) => x ! Clock(value)
        case _ => print("Should never happen environment has no anthill")
      }

    case Move(position: Vector2D, delta: Vector2D) =>
      val newPosition = position >> delta
      if (state.boundary.hasInside(newPosition)) {
        if (state.obstacles.forall(!_.hasInside(newPosition)))
          sender ! NewPosition(newPosition, newPosition - position)
        else {
          /* If ant is moving outside boundary or through an obstacle, invert its new position */
          val collision = state.obstacles.find(_.hasInside(newPosition)).get
          collision match {
            case f: Food =>
              sender ! FoodNear
              context become defaultBehaviour(state.updateFood(f, f - ConstantInsectInfo.MAX_FOOD))
            case x =>
              val intersectionAndDirection = x.findIntersectionPoint(position, newPosition)
              //println(intersectionAndDirection)
              val newDelta = intersectionAndDirection.intersectionPoint - newPosition
              sender ! NewPosition(intersectionAndDirection.intersectionPoint >> newDelta, newDelta)
          }
        }
      } else sender ! NewPosition(position - delta, delta -)

    case UpdateInsect(info: InsectInfo) =>
      val updatedInfo = state.updateAntsInfo(info)

      /* When all ants return their positions, environment send them to GUI */
      if (updatedInfo.antsInfo.size == state.ants.size) {
        state.gui.get ! Repaint(updatedInfo.antsInfo ++ updatedInfo.obstacles ++ Seq(state.anthillInfo))
        context become defaultBehaviour(state.emptyAntsInfo())
      } else context become defaultBehaviour(updatedInfo)

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context become defaultBehaviour(state.updateAnthillInfo(anthillInfo))


    //TODO next sprint
    /*case AddRandomAnt(nAnts: Int, step: String) =>
      val randomPosition = RandomVector2D(state.boundary.topLeft.x, state.boundary.topRight.x)
      val antInfo = ForagingAntInfo(state.anthill.get,
        time = step.toInt + nAnts, id = state.ants.size + nAnts, position = randomPosition)
      val ants = createAntByUser(antInfo)
      context become defaultBehaviour(state.createAnt(ants, antInfo)) */

    case KillAnt(id: Int) =>
      log.debug("Kill")
      context.stop(state.ants(id))
      val newData = state.removeAnt(id)
      if (newData.ants.isEmpty) state.gui.get ! Repaint(state.obstacles ++ Seq(state.anthillInfo))
      context become defaultBehaviour(newData)
  }

  /** Returns ants references, created from random position */
  private def createAntFromRandomPosition(nAnts: Int, anthill: ActorRef): Map[Int, ActorRef] =
    (0 until nAnts).map(i => {
      val randomPosition = RandomVector2D(state.boundary.topLeft.x, state.boundary.topRight.x)
      i -> context.actorOf(ForagingAnt(ForagingAntInfo(anthill, id = i, position = randomPosition), self), s"ant-$i")
    }).toMap

  /** Returns ants references, created from the center of boundary */
  private def createAntFromCenter(nAnts: Int, anthill: ActorRef): Map[Int, ActorRef] =
    (0 until nAnts).map(i => {
      val center = state.boundary.center
      i -> context.actorOf(ForagingAnt(ForagingAntInfo(anthill, id = i, position = center), self), s"ant-$i")
    }).toMap

  /** Returns ants references, created from intention of user. The ant start in RandomPosition */
  private def createAntByUser(antInfo: InsectInfo): ActorRef = {
    context.actorOf(ForagingAnt(antInfo, self), s"ant-${antInfo.id}")
  }
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
