package model.environment

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import model.insects.{ConstantInsectInfo, ForagingAnt, ForagingAntInfo, InsectInfo}
import utility.Geometry._
import utility.Messages.{Clock, Move, StartSimulation, UpdateInsect, _}
import model.BorderedEntityFactory._
import model.Food
import model.anthill.{Anthill, AnthillInfo}

import scala.util.Random

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
      if (Random.nextDouble() < 0.01) self ! AntBirth(value)

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
      if (updatedInfo.antsInfo.size == updatedInfo.ants.size) {
        state.gui.get ! Repaint(updatedInfo.antsInfo ++ updatedInfo.obstacles ++ Seq(state.anthillInfo))
        context become defaultBehaviour(state.emptyAntsInfo())
      } else context become defaultBehaviour(updatedInfo)

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context become defaultBehaviour(state.updateAnthillInfo(anthillInfo))

    case AntBirth(clock: Int) =>
      println("Birth")
      val antId = state.ants.size
      val birthPosition = state.anthillInfo.position
      val ant = context.actorOf(ForagingAnt(ForagingAntInfo(state.anthill.get, id = antId, position = birthPosition, time = clock - 1), self), s"ant-$antId")
      ant ! Clock(clock)
      context become defaultBehaviour(state.addAnt(antId, ant))

    case KillAnt(id: Int) =>
      context.stop(sender)
      val newData = state.removeAnt(id)
      if (newData.ants.isEmpty) state.gui.get ! Repaint(state.obstacles ++ Seq(state.anthillInfo))
      if (newData.antsInfo.size == newData.ants.size) {
        state.gui.get ! Repaint(newData.antsInfo ++ newData.obstacles ++ Seq(state.anthillInfo))
        context become defaultBehaviour(newData.emptyAntsInfo())
      } else context become defaultBehaviour(newData)
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
