package model.environment

import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef, Props}
import model.Fights.Fight
import model.anthill.{Anthill, AnthillInfo}
import model.insects._
import utility.geometry._
import utility.Messages._
import utility.PheromoneSeq._
import model.environment.elements.{EnvironmentElements, Food, Obstacle}
import EnvironmentElements._
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

      val obstacles = Seq.empty
        /*if (obstaclesPresence.isDefined) (0 until obstaclesPresence.get).map(_ =>
        createRandomSimpleObstacle(200, 600)) else Seq.empty*/

      val foods = if (foodPresence.isDefined) (0 until foodPresence.get).map(_ =>
        Food.createRandomFood(state.boundary.topLeft.x, state.boundary.bottomRight.x)) else Seq.empty

      val foodPheromones = Seq(FoodPheromone(RandomVector2DInSquare(100, 200), 0.3, 5.4))
      context >>> defaultBehaviour(EnvironmentInfo(Some(sender), state.boundary,
         obstacles, foods,  ants, enemies, anthill, state.anthillInfo, foodPheromones))

    case AddFoodPheromone(pheromone: FoodPheromone, threshold: Double) =>
      context >>> defaultBehaviour(state.addPheromone(pheromone, threshold))


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
      context >>> defaultBehaviour(state.updatePheromones(state.pheromones.tick()))


    case Move(position: Vector2D, delta: Vector2D) =>
      val newPosition = position >> delta

      import model.environment.elements.EnvironmentElements.BoundaryHasInside

      /*Checking boundary*/
      if (checkHasInside(state.boundary, newPosition)) {

        /*Checking obstacles*/
        import EnvironmentElements.ObstacleHasInside
        val obstacle = checkHaveInside(state.obstacles, newPosition)
        if (obstacle.isDefined) {
          val intersection = handleObstacleIntersection(obstacle.get, position, newPosition)
          sender ! NewPosition(intersection._1, intersection._2)

        } else {

          /*Checking food sources*/
          import EnvironmentElements.FoodHasInside
          val food = checkHaveInside(state.foods, newPosition)
          if (food.isDefined) {
            sender ! FoodNear(food.get.position)
            sender ! NewPosition(position , ZeroVector2D())
          } else {
            sender ! NewPosition(newPosition, newPosition - position)
          }
        }
      } else sender ! NewPosition(position - delta, delta -)

    case TakeFood(delta, position) =>
      import EnvironmentElements.FoodHasInside
      val food = checkHaveInside(state.foods, position)
      if (food.isDefined) {
        sender ! TakeFood(delta, position)
        context >>> defaultBehaviour(state.updateFood(food.get, food.get - delta))
      } else {
        sender ! TakeFood(0, position)
      }

    case UpdateInsect(info: InsectInfo) =>
      sendInfoToGUI(state.updateInsectInfo(info))

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context >>> defaultBehaviour(state.updateAnthillInfo(anthillInfo))

    case AntBirth(clock: Int) =>
      /*Generate an id that doesn't exist for sure to avoid name conflicts*/
      val antId = state.ants.size + clock
      val birthPosition = state.anthillInfo.position
      val ant = context.actorOf(ForagingAnt(ForagingAntInfo(state.anthill.get, id = antId, position = birthPosition, time = clock - 1), self), s"ant-$antId")
      ant ! Clock(clock)
      context >>> defaultBehaviour(state.addAnt(antId, ant))

    case KillAnt(id: Int) =>
      context.stop(sender)
      val newData = state.removeAnt(id)
      if (newData.ants.isEmpty) state.gui.get ! Repaint(state.obstacles ++ state.foods ++ Seq(state.anthillInfo))
      sendInfoToGUI(newData)
  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become behaviour
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

  private def handleObstacleIntersection(obstacle: Obstacle, position: Vector2D, newPosition: Vector2D): (Vector2D, Vector2D) = {
    val intersectionAndDirection = obstacle.findIntersectionPoint(position, newPosition).head
    val angleTest = if (intersectionAndDirection.angle < math.Pi / 2) math.Pi - (intersectionAndDirection.angle * 2)
    else - ((2 * intersectionAndDirection.angle) - math.Pi)
    val newDelta = intersectionAndDirection.intersectionPoint - newPosition
    val orientedDelta = (
      (math.cos(angleTest) * newDelta.x) - (math.sin(angleTest) * newDelta.y),
      (math.sin(angleTest) * newDelta.x) + (math.cos(angleTest) * newDelta.y)
    )
    import TupleOp2._
    (intersectionAndDirection.intersectionPoint >> orientedDelta, orientedDelta)

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
          case Left(x) =>
            context.stop(info.ants(x.id))
            updatedInfo = updatedInfo.removeAnt(x.id)
          case Right(x) =>
            context.stop(info.enemies(x.id))
            updatedInfo = updatedInfo.removeEnemy(x.id)
        }
      }
      info.gui.get ! Repaint(info.antsInfo ++ info.enemiesInfo ++
        info.obstacles ++ info.foods ++ Seq(info.anthillInfo) ++ info.pheromones ++ fights)
      context >>> defaultBehaviour(updatedInfo.emptyInsectInfo())
    } else context >>> defaultBehaviour(info)
  }

  private def checkFights(antsInfo: Iterable[ForagingAntInfo], enemiesInfo: Iterable[EnemyInfo]): Iterable[Fight[ForagingAntInfo, EnemyInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
