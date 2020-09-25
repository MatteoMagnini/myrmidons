package model.environment

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import model.Fights.Fight
import model.anthill.{Anthill, AnthillInfo}
import model.insects._
import utility.Geometry.{RandomVector2DInSquare, Vector2D}
import utility.Messages._
import utility.PheromoneSeq._
import model.BorderedEntityFactory._

import scala.util.Random

/** Environment actor
 *
 * @param state environment internal state
 */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, nEnemies: Int, centerSpawn: Boolean, obstaclesPresence, foodPresence) =>

      val anthill = context.actorOf(Anthill(AnthillInfo(state.boundary.center, 15, foodAmount = 5000), self), name = "anthill")
      val ants = if (!centerSpawn) createAntFromRandomPosition(nAnts, anthill) else createAntFromCenter(nAnts, anthill)

      // anthill ref is need to permit the enemies to interact with anthill (parasite behaviour)
      val enemies = createEnemiesFromRandomPosition(nEnemies, anthill)

      val obstacles = if (obstaclesPresence.isDefined) (0 until obstaclesPresence.get).map(_ =>
        createRandomSimpleObstacle(200, 600)) else Seq.empty

      val foods = if (foodPresence.isDefined) (0 until foodPresence.get).map(_ =>
        createRandomFood(state.boundary.topLeft.x, state.boundary.bottomRight.x)) else Seq.empty

      val foodPheromones = Seq(FoodPheromone(RandomVector2DInSquare(100, 200), 0.3, 5.4))
      context become defaultBehaviour(EnvironmentInfo(Some(sender), state.boundary,
         obstacles, foods,  ants, enemies, anthill, state.anthillInfo, foodPheromones))

    case AddFoodPheromone(pheromone: FoodPheromone, threshold: Double) =>
      context become defaultBehaviour(state.addPheromone(pheromone, threshold))


    case Clock(value: Int) =>
      /* Random birth of ants */
      if (Random.nextDouble() < 0.01) self ! AntBirth(value)
      state.ants.values.foreach(_ ! Clock(value))
      state.enemies.values.foreach(_ ! Clock(value))
      state.ants.values.foreach(_ ! FoodPheromones(state.pheromones))
      state.anthill match {
        case Some(x) => x ! Clock(value)
        case _ => print("Should never happen environment has no anthill")
      }
      context become defaultBehaviour(state.updatePheromones(state.pheromones.tick()))

    case Move(position: Vector2D, delta: Vector2D) =>
      val newPosition = position >> delta
      if (state.boundary.hasInside(newPosition)) {
        val obstacle = state.obstacles.find(_.hasInside(newPosition))
        if (obstacle.isDefined) {

          val intersectionAndDirection = obstacle.get.findIntersectionPoint(position, newPosition)
          //println(intersectionAndDirection)
          val newDelta = intersectionAndDirection.intersectionPoint - newPosition
          sender ! NewPosition(intersectionAndDirection.intersectionPoint >> newDelta, newDelta)
        } else {
          val food = state.foods.find(_.hasInside(newPosition))
          if (food.isDefined) {
            sender ! FoodNear(food.get.position)
            //TODO: code replication!!!
            val intersectionAndDirection = food.get.findIntersectionPoint(position, newPosition)
            //println(intersectionAndDirection)
            val newDelta = intersectionAndDirection.intersectionPoint - newPosition
            sender ! NewPosition(intersectionAndDirection.intersectionPoint >> newDelta, newDelta)
          } else {
            sender ! NewPosition(newPosition, newPosition - position)
          }

        }
      } else sender ! NewPosition(position - delta, delta -)

    case TakeFood(delta, position) =>
      val food = state.foods.find(_.hasInside(position))
      if (food.nonEmpty) {
        sender ! TakeFood(delta, position)
        context become defaultBehaviour(state.updateFood(food.get, food.get - delta))
      } else {
        sender ! TakeFood(0, position)
      }

    case UpdateInsect(info: InsectInfo) =>
      sendInfoToGUI(state.updateInsectInfo(info))

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context become defaultBehaviour(state.updateAnthillInfo(anthillInfo))

    case AntBirth(clock: Int) =>
      val antId = state.ants.size + clock
      val birthPosition = state.anthillInfo.position
      val ant = context.actorOf(ForagingAnt(ForagingAntInfo(state.anthill.get, id = antId, position = birthPosition, time = clock - 1), self), s"ant-$antId")
      ant ! Clock(clock)
      context become defaultBehaviour(state.addAnt(antId, ant))

    case KillAnt(id: Int) =>
      context.stop(sender)
      val newData = state.removeAnt(id)
      if (newData.ants.isEmpty) state.gui.get ! Repaint(state.obstacles ++ state.foods ++ Seq(state.anthillInfo))
      sendInfoToGUI(newData)
  }

  /** Returns ants references, created from random position */
  private def createAntFromRandomPosition(nAnts: Int, anthill: ActorRef): Map[Int, ActorRef] =
    (0 until nAnts).map(i => {
      val randomPosition = RandomVector2DInSquare(state.boundary.topLeft.x, state.boundary.topRight.x)
      i -> context.actorOf(ForagingAnt(ForagingAntInfo(anthill, id = i, position = randomPosition), self), s"ant-$i")
    }).toMap

  /** Returns ants references, created from the center of boundary */
  private def createAntFromCenter(nAnts: Int, anthill: ActorRef): Map[Int, ActorRef] =
    (0 until nAnts).map(i => {
      val center = state.boundary.center
      i -> context.actorOf(ForagingAnt(ForagingAntInfo(anthill, id = i, position = center), self), s"ant-$i")
    }).toMap

  /** Returns enemies references, created from random position */
  private def createEnemiesFromRandomPosition(nEnemies: Int, anthill: ActorRef): Map[Int, ActorRef] =
    (0 until nEnemies).map(i => {
      val randomPosition = RandomVector2DInSquare(state.boundary.topLeft.x, state.boundary.topRight.x)
      i -> context.actorOf(Enemy(EnemyInfo(anthill, id = i, position = randomPosition), self), s"enemy-$i")
    }).toMap

  /** Returns ants references, created from intention of user. The ant start in RandomPosition */
  private def createAntByUser(antInfo: InsectInfo): ActorRef = {
    context.actorOf(ForagingAnt(antInfo, self), s"ant-${antInfo.id}")
  }

  private def sendInfoToGUI(info: EnvironmentInfo): Unit = {
    import model.Fights._
    import model.Fights.InsectFight._

    /* When all insects return their positions, environment send them to GUI */
    if ((info.antsInfo.size == info.ants.size) && (info.enemiesInfo.size == info.enemies.size)) {
      var updatedInfo = info
      val fights = checkFights(info.antsInfo, info.enemiesInfo)
      for (loser <- losers(fights)) {
        loser match {
          case x:ForagingAntInfo =>
            context.stop(info.ants(x.id))
            updatedInfo = updatedInfo.removeAnt(x.id)
          case x:EnemyInfo =>
            context.stop(info.enemies(x.id))
            updatedInfo = updatedInfo.removeEnemy(x.id)
        }
      }
      info.gui.get ! Repaint(info.antsInfo ++ info.enemiesInfo ++
        info.obstacles ++ info.foods ++ Seq(info.anthillInfo) ++ info.pheromones ++ fights)
      context become defaultBehaviour(updatedInfo.emptyInsectInfo())
    } else context become defaultBehaviour(info)
  }

  private def checkFights(antsInfo: Iterable[InsectInfo], enemiesInfo: Iterable[InsectInfo]): Iterable[Fight[InsectInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
