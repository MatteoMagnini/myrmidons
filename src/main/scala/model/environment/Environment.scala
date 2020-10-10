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
import utility.geometry.{RandomVector2DInSquare, Vector2D, ZeroVector2D}
import model.environment.elements.EnvironmentElements._
import utility.Parameters.Environment._
import model.environment.pheromones.{DangerPheromone, FoodPheromone}
import Implicits._

import scala.util.Random

/** Environment actor
 *
 * @param state environment internal state
 */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = initializationBehaviour(state)

  private def initializationBehaviour(state:EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, nEnemies: Int, obstaclesPresence, foodPresence) =>
      val anthillInfo = AnthillInfo(state.boundary.center, ANTHILL_RADIUS , FOOD_AMOUNT)
      val anthill = context.actorOf(Anthill(anthillInfo, self), name = "anthill")
      anthill ! CreateEntities(nAnts, 0.5)

      def randomPositionOutObstacle(obstacleList: Seq[Obstacle], minMax:(Double,Double)):Vector2D = {
        import model.environment.elements.EnvironmentElements.ObstacleHasInside
        var randomPosition = ZeroVector2D()
        do{
          randomPosition = RandomVector2DInSquare(minMax._1, minMax._2)
        }
        while(checkHaveInside(obstacleList, randomPosition).nonEmpty)
        randomPosition
      }

      val obstacles = if (obstaclesPresence.isDefined) Obstacle.createRandom(obstaclesPresence.get, anthillInfo.position, (50,150)).toSeq
      else Seq.empty

      val enemies = (0 until nEnemies).map(i => {
        val randomPosition = randomPositionOutObstacle(obstacles, (state.boundary.topLeft.x, state.boundary.topRight.x))
        i -> context.actorOf(Enemy(EnemyInfo(id = i, position = randomPosition), self), s"enemy-$i")
      }).toMap

      val foods = if (foodPresence.isDefined) (0 until foodPresence).map(_ =>
        Food.createRandomFood(anthillInfo.position, FOOD_RADIUS._1, FOOD_RADIUS._2)) else Seq.empty

      context >>> initializationBehaviour(EnvironmentInfo(Some(sender), state.boundary,
        obstacles, foods, anthill, Some(anthillInfo)).addEnemies(enemies))

    case NewEntities(ants: Map[Int, ActorRef]) =>
      state.gui.get ! Ready
      context >>> defaultBehaviour(state.addAnts(ants))
  }

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case Clock(value: Int) =>
      if (Random.nextDouble() < 0.1) {
        self ! AntBirth(value)
      }
      state.ants.values.foreach(_ ! Clock(value))
      state.ants.values.foreach(_ ! FoodPheromones(state.foodPheromones))
      state.ants.values.foreach(_ ! DangerPheromones(state.dangerPheromones))
      state.enemies.values.foreach(_ ! Clock(value))
      state.anthill.get ! Clock(value)
      val newData = state.updateDangerPheromones(state.dangerPheromones.tick())
      context >>> defaultBehaviour(newData.updateFoodPheromones(state.foodPheromones.tick()))

    case Move(position: Vector2D, delta: Vector2D) =>
      CollisionsInterceptor.checkCollisions(self, sender, state, position, delta)

    case TakeFood(delta, position) =>
      val food = state.foods.filter(f => f.position ~~ (position, 1E-7))
      if (food.nonEmpty) {
        sender ! TakeFood(delta, position)
        val foo = food.head
        val upFoo = foo-delta
        context >>> defaultBehaviour(state.updateFood(foo, upFoo))
      } else {
        sender ! TakeFood(0, position)
      }

    case UpdateInsect(info: SpecificInsectInfo[x]) =>
      val newState = state.updateInsectInfo(info)
      if ((newState.foragingAntsInfo.size + newState.patrollingAntsInfo.size == newState.ants.size)
        && (newState.enemiesInfo.size == newState.enemies.size)) {
        sendInfoToGUI(newState)
      }  else context >>> defaultBehaviour(newState)

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context >>> defaultBehaviour(state.updateAnthillInfo(Some(anthillInfo)))

    case AntBirth(clock: Int) =>
      val antId = state.maxAntId + 1
      val birthPosition = state.anthillInfo.position
      val ant = if(math.random()< 0.3)
        context.actorOf(PatrollingAnt(PatrollingAntInfo(state.anthill, id = antId, position = birthPosition, time = clock-1), self), s"ant-$antId")
      else
        context.actorOf(ForagingAnt(ForagingAntInfo(state.anthill, id = antId, position = birthPosition, time = clock-1), self), s"ant-$antId")

      ant ! Clock(clock)
      context >>> defaultBehaviour(state.addAnt(antId, ant))

    case KillInsect(info: InsectInfo) =>
      context.stop(sender)
      val newState = state.removeInsect(info)
      if ((newState.foragingAntsInfo.size + newState.patrollingAntsInfo.size == newState.ants.size)
        && (newState.enemiesInfo.size == newState.enemies.size)) {
        sendInfoToGUI(newState)
      } else context >>> defaultBehaviour(newState)
      //

    case AddFoodPheromone(pheromone: FoodPheromone, threshold: Double) =>
      context >>> defaultBehaviour(state.addFoodPheromone(pheromone, threshold))

    case AddDangerPheromone(pheromone: DangerPheromone, threshold: Double) =>
      context >>> defaultBehaviour(state.addDangerPheromone(pheromone, threshold))
  }

  private def sendInfoToGUI(info: EnvironmentInfo): Unit = {
    val fights = findFights(info.foragingAntsInfo ++ info.patrollingAntsInfo, info.enemiesInfo)
    val updatedInfo = handleFights(info, fights)

    info.gui.get ! Repaint(info.anthillInfo.get +: (info.foragingAntsInfo ++ info.patrollingAntsInfo ++ info.enemiesInfo ++
      info.obstacles ++ info.foods ++ info.foodPheromones ++ info.dangerPheromones ++ fights).toSeq)
    context >>> defaultBehaviour(updatedInfo.emptyInsectInfo())
  }

  private def findFights(antsInfo: Iterable[InsectInfo], enemiesInfo: Iterable[EnemyInfo]): Iterable[Fight[InsectInfo, EnemyInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)

  private def handleFights(info: EnvironmentInfo, fights: Iterable[Fight[InsectInfo, EnemyInfo]]): EnvironmentInfo = {

    import model.Fights._
    import model.Fights.InsectFight._
    import Implicits._
    var updatedInfo = info
    for (loser <- losers(fights)) {
      loser match {
        case Left(ant) =>
          info.ants(ant.id) ! KillInsect(ant)
        case Right(enemy) =>
          context.stop(info.enemies(enemy))
          updatedInfo = updatedInfo.removeEnemy(enemy)
      }
    }
    updatedInfo
  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become behaviour
  }
}


object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}

object Implicits {
  implicit def extractOption[X](value: Option[X]): X = value.get

  implicit def entity2Id[X <: SpecificInsectInfo[X]](entity: X): Int = entity.id
}