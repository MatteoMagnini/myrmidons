package model.environment

import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef, Props}
import model.Fights.Fight
import model.anthill.{Anthill, AnthillInfo}
import model.environment.elements.{Food, Obstacle}
import model.insects._
import model.insects.info._
import utility.Messages._
import utility.PheromoneSeq._
import model.insects.info.SpecificInsectInfo
import utility.geometry.{RandomVector2DInCircle, RandomVector2DInSquare, Vector2D, ZeroVector2D}
import model.environment.elements.EnvironmentElements._
import scala.util.Random

/** Environment actor
 *
 * @param state environment internal state
 */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, nEnemies: Int, spawnFromAnthill: Boolean, obstaclesPresence, foodPresence) =>
      import Implicits._
      val anthillInfo = AnthillInfo(state.boundary.center, 15, foodAmount = 2000)
      val anthill = context.actorOf(Anthill(anthillInfo, self), name = "anthill")
      val entities = if (!spawnFromAnthill) createEntitiesFromRandomPosition(nAnts, nEnemies, anthill)
      else createAntFromAnthill(nAnts, nEnemies, anthill, anthillInfo.position)

      val obstacles = if (obstaclesPresence.isDefined) for {_ <- 0 until obstaclesPresence.get
                                                            random = Obstacle.randomValid
                                                            obstacle = Obstacle(RandomVector2DInCircle(50, 350,
                                                              anthillInfo.position), 20, random)
                                                            } yield obstacle
      else Seq.empty

      val foods = if (foodPresence.isDefined) (0 until foodPresence).map(_ =>
        Food.createRandomFood(anthillInfo.position, 100, 150)) else Seq.empty

      context >>> defaultBehaviour(EnvironmentInfo(Some(sender), state.boundary,
         obstacles, foods, entities._1, entities._2, entities._3, anthill, Some(anthillInfo)))

    case Clock(value: Int) =>
      state.foragingAnts.values.foreach(_ ! Clock(value))
      state.patrollingAnts.values.foreach(_ ! Clock(value))
      state.enemies.values.foreach(_ ! Clock(value))
      state.foragingAnts.values.foreach(_ ! FoodPheromones(state.pheromones))
      state.anthill match {
        case Some(x) => x ! Clock(value)
        case _ => print("Should never happen environment has no anthill")
      }
      context >>> defaultBehaviour(state.updatePheromones(state.pheromones.tick()))


    case Move(position: Vector2D, delta: Vector2D) =>

      val newPosition = position >> delta

      /*Checking boundary*/
      import model.environment.elements.EnvironmentElements.BoundaryHasInside
      if (checkHasInside(state.boundary, newPosition)) {
        /*Checking obstacles*/
        import model.environment.elements.EnvironmentElements.ObstacleHasInside
        val obstacle = checkHaveInside(state.obstacles, newPosition)
        if (obstacle.nonEmpty) {
          // TODO: better name
          val bouncedPosition = recursionCheck(obstacle, position, newPosition)
          sender ! NewPosition(bouncedPosition._1, bouncedPosition._2)

        } else {

          /*Checking food sources*/
          import model.environment.elements.EnvironmentElements.FoodHasInside
          val food = checkHaveInside(state.foods, newPosition)

          if (food.nonEmpty) {
            val t = recursionCheck(food, position, newPosition)
            val nearestFood: Food = food.toList.sortWith((a, b) =>  //TODO: Not correct in all case
                position --> a.position < position --> b.position).head
            sender ! FoodNear(nearestFood.position)
            sender ! NewPosition(t._1 , t._2) // TODO: should bounce also on food!

          } else {
            sender ! NewPosition(newPosition, newPosition - position)
          }
        }
      } else sender ! NewPosition(position - delta, delta -)

    case TakeFood(delta, position) =>
      import Implicits._
      import model.environment.elements.EnvironmentElements.FoodHasInside

      val food = checkHaveInside(state.foods, position)
      if (food.nonEmpty) {
        sender ! TakeFood(delta, position)
        val nearestFood: Food = food.toList.sortWith((a, b) =>
          position --> a.position < position --> b.position).head
        context >>> defaultBehaviour(state.updateFood(nearestFood, nearestFood - delta))
      } else {
        sender ! TakeFood(0, position)
      }

    case UpdateInsect(info: SpecificInsectInfo[x]) =>
      sendInfoToGUI(state.updateInsectInfo(info))

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context >>> defaultBehaviour(state.updateAnthillInfo(Some(anthillInfo)))

    case AntBirth(clock: Int) =>
      import Implicits._
      /*Generate an id that doesn't exist for sure to avoid name conflicts*/
      val antId = state.foragingAnts.size + clock
      val birthPosition = state.anthillInfo.position
      val ant = context.actorOf(ForagingAnt(ForagingAntInfo(state.anthill, id = antId, position = birthPosition, time = clock - 1), self), s"ant-$antId")
      ant ! Clock(clock)
      context >>> defaultBehaviour(state.addAnt(antId, ant))

    case KillInsect(info: InsectInfo) =>
      context.stop(sender)
      val newData = state.removeInsect(info)
      sendInfoToGUI(newData)


    case AddFoodPheromone(pheromone: FoodPheromone, threshold: Double) =>
      context >>> defaultBehaviour(state.addPheromone(pheromone, threshold))

  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become behaviour
  }

  /** Returns ants and enemies references, created from random position */
  private def createEntitiesFromRandomPosition(nAnts: Int, nEnemies: Int, anthill: ActorRef): (Map[Int, ActorRef], Map[Int, ActorRef], Map[Int, ActorRef]) = {
    val foragingAnts = (0 until nAnts).map(i => {
      val randomPosition = RandomVector2DInSquare(state.boundary.topLeft.x, state.boundary.topRight.x)
      i -> context.actorOf(ForagingAnt(ForagingAntInfo(anthill, id = i, position = randomPosition), self), s"ant-$i")
    }).toMap

    val patrollingAnts = (0 until nAnts).map(i => {
      val randomPosition = RandomVector2DInSquare(state.boundary.topLeft.x, state.boundary.topRight.x)
      i -> context.actorOf(PatrollingAnt(PatrollingAntInfo(anthill, id = i, position = randomPosition), self), s"p-ant-$i")
    }).toMap

    val enemies = (0 until nEnemies).map(i => {
      val randomPosition = RandomVector2DInSquare(state.boundary.topLeft.x, state.boundary.topRight.x)
      i -> context.actorOf(Enemy(EnemyInfo(id = i, position = randomPosition), self), s"enemy-$i")
    }).toMap

    (foragingAnts, patrollingAnts, enemies)
  }

  /** Returns ants and enemies references, creating ants from the center of boundary */
  private def createAntFromAnthill(nAnts: Int, nEnemies:Int, anthill: ActorRef, anthillCenter: Vector2D): (Map[Int, ActorRef], Map[Int, ActorRef], Map[Int, ActorRef]) = {
    val foragingAnts = (0 until nAnts).map(i => {
      i -> context.actorOf(ForagingAnt(ForagingAntInfo(anthill, id = i, position = anthillCenter), self), s"f-ant-$i")
    }).toMap
    val patrollingAnts = (0 until nAnts).map(i => {
      i -> context.actorOf(PatrollingAnt(PatrollingAntInfo(anthill, id = i, position = anthillCenter), self), s"p-ant-$i")
    }).toMap
    val enemies = (0 until nEnemies).map(i => {
      val randomPosition = RandomVector2DInSquare(state.boundary.topLeft.x, state.boundary.topRight.x)
      i -> context.actorOf(Enemy(EnemyInfo(id = i, position = randomPosition), self), s"enemy-$i")
    }).toMap
    (foragingAnts, patrollingAnts, enemies)
  }

  private def recursionCheck(obstacle: Iterable[Obstacle], position: Vector2D, newPosition: Vector2D):(Vector2D, Vector2D) = {
    val res = handleObstacleIntersection(obstacle, position, newPosition)
    val intersection = res._1
    val delta = res._2

    val bouncedPos = intersection >> delta

    val o = checkHaveInside(state.obstacles, bouncedPos)
    if (o.isEmpty) {
      (bouncedPos, delta)
    } else {
      recursionCheck(o, intersection, bouncedPos)
    }
  }

  private def handleObstacleIntersection(obstacle: Iterable[Obstacle], position: Vector2D, newPosition: Vector2D): (Vector2D, Vector2D) = {
  import Implicits._

    val intersectionOnOverlappedObstacle = (for (f <- obstacle)
      yield f.findIntersectionInformation(position, newPosition)).toList

    val intersectionAndDirectionOpt = intersectionOnOverlappedObstacle
      .filter(_.nonEmpty)
      .sortWith((a, b) => position --> a.intersectionPoint < position --> b.intersectionPoint).headOption

    if (intersectionAndDirectionOpt.nonEmpty) {
      val intersectionAndDirection = intersectionAndDirectionOpt.get
      val angleTest = if (intersectionAndDirection.angle < math.Pi / 2)
        math.Pi - (intersectionAndDirection.angle * 2)
      else
        -((2 * intersectionAndDirection.angle) - math.Pi)
      val newDelta = intersectionAndDirection.intersectionPoint - newPosition
      val orientedDelta = (
        (math.cos(angleTest) * newDelta.x) - (math.sin(angleTest) * newDelta.y),
        (math.sin(angleTest) * newDelta.x) + (math.cos(angleTest) * newDelta.y)
      )
      import utility.geometry.TupleOp2._
      (intersectionAndDirection.intersectionPoint, orientedDelta)
    } else {
      (position, (position - newPosition)/(((position - newPosition)|| )/4))
    }

  }

  private def sendInfoToGUI(info: EnvironmentInfo): Unit = {
    /* When all insects return their positions, environment send them to GUI */

    if ((info.foragingAntsInfo.size == info.foragingAnts.size)
      && (info.patrollingAntsInfo.size == info.patrollingAnts.size)
      && (info.enemiesInfo.size == info.enemies.size)) {
      val fights = findFights(info.foragingAntsInfo, info.enemiesInfo)
      val updatedInfo = handleFights(info, fights)

      info.gui.get ! Repaint(info.anthillInfo.get +: (info.foragingAntsInfo ++ info.patrollingAntsInfo ++ info.enemiesInfo ++
        info.obstacles ++ info.foods ++ info.pheromones ++ fights).toSeq)
      context >>> defaultBehaviour(updatedInfo.emptyInsectInfo())
    } else context >>> defaultBehaviour(info)
  }

  private def findFights(antsInfo: Iterable[ForagingAntInfo], enemiesInfo: Iterable[EnemyInfo]): Iterable[Fight[ForagingAntInfo, EnemyInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)

  private def handleFights(info: EnvironmentInfo, fights: Iterable[Fight[ForagingAntInfo, EnemyInfo]]): EnvironmentInfo = {

    import model.Fights._
    import model.Fights.InsectFight._
    import Implicits._

    var updatedInfo = info
    for (loser <- losers(fights)) {
      loser match {
        case Left(ant) =>
          context.stop(info.foragingAnts(ant))
          updatedInfo = updatedInfo.removeAnt(ant)
        case Right(enemy) =>
          context.stop(info.enemies(enemy))
          updatedInfo = updatedInfo.removeEnemy(enemy)
      }
    }
    updatedInfo
  }
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}

object Implicits {
  implicit def extractOption[X](value: Option[X]): X = value.get

  implicit def entity2Id[X <: SpecificInsectInfo[X]](entity: X): Int = entity.id
}