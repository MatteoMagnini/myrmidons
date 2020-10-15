package model.environment

import akka.actor.{Actor, ActorLogging, Props}
import model.Fights.Fight
import model.environment.anthill.{Anthill, AnthillInfo}
import model.environment.elements.EnvironmentElements._
import model.environment.elements.{Food, Obstacle}
import model.environment.pheromones.Pheromone
import model.environment.info.{EnvironmentInfo, InsectReferences}
import model.insects.Ants.ForagingAnt._
import model.insects._
import model.insects.info.{SpecificInsectInfo, _}
import utility.Messages._
import utility.PheromoneMap._
import utility.geometry.{RandomVector2DInSquare, Vector2D, ZeroVector2D}
import utility.RichActor._

import scala.util.Random

/** Environment actor
 *
 * @param state environment internal state
 */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = initializationBehaviour(state)

  def randomPositionOutObstacle(obstacleList: Seq[Obstacle], minMax:(Double,Double)):Vector2D = {
    import model.environment.elements.EnvironmentElements.ObstacleHasInside
    var randomPosition = ZeroVector2D()
    do{
      randomPosition = RandomVector2DInSquare(minMax._1, minMax._2)
    }
    while(checkHaveInside(obstacleList, randomPosition).nonEmpty)
    randomPosition
  }

  private def initializationBehaviour(state:EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, nEnemies: Int, obstaclesPresence, foodPresence) =>
      val anthillInfo = AnthillInfo(state.boundary.center, ANTHILL_RADIUS , FOOD_AMOUNT)
      val anthill = context.actorOf(Anthill(anthillInfo, self), name = "anthill")
      anthill ! CreateEntities(nAnts, FORAGING_PERCENTAGE)

      val obstacles = if (obstaclesPresence.isDefined) {
        Obstacle.createRandom(obstaclesPresence.get, anthillInfo.position, (50,150)).toSeq
      }
      else {
        Seq.empty
      }

      val foods = if (foodPresence.isDefined) {
        (0 until foodPresence).map(_ =>
          Food.createRandomFood(anthillInfo.position, FOOD_RADIUS._1, FOOD_RADIUS._2))
      } else {
        Seq.empty
      }

      val enemies = (0 until nEnemies).map(i => {
        val randomPosition = randomPositionOutObstacle(obstacles ++ foods,
          (state.boundary.topLeft.x, state.boundary.topRight.x))
        i -> context.actorOf(Enemy(EnemyInfo(id = i, position = randomPosition), self), s"enemy-$i")
      }).toMap


      context >>> initializationBehaviour(EnvironmentInfo(Some(sender), state.boundary,
        obstacles, foods, anthill, Some(anthillInfo)).addEnemies(enemies))

    case NewEntities(ants: InsectReferences) =>
      state.gui.get ! Ready
      context >>> defaultBehaviour(state.addAnts(ants))
  }

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case Clock(value: Int) =>
      val antHillFoodPercentage = state.anthillInfo.get.foodAmount / state.anthillInfo.get.maxFoodAmount
      val scaleFactor = 1 / MAX_FOOD
      if (Random.nextDouble() < (antHillFoodPercentage * scaleFactor)) {
        self ! AntBirth(value)
      }
      state.ants.values.foreach(_ ! Clock(value))
      state.ants.values.foreach(_ ! Pheromones(state.pheromones, state.tree))
      state.enemies.values.foreach(_ ! Clock(value))
      state.anthill.get ! Clock(value)
      val newData = state.updatePheromones(state.pheromones.tick())
      context >>> defaultBehaviour(newData)

    case Move(position: Vector2D, delta: Vector2D) =>
      CollisionsInterceptor.checkCollisions(sender, state, position, delta)

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
      }  else {context >>> defaultBehaviour(newState)}

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context >>> defaultBehaviour(state.updateAnthillInfo(Some(anthillInfo)))

    case AntBirth(clock: Int) => context >>> defaultBehaviour(createNewAnt(clock, state, 0.3f))

    case KillInsect(info: InsectInfo) => killInsect(info, state)

    case AddPheromone(pheromone: Pheromone, threshold: Double) =>
      context >>> defaultBehaviour(state.addPheromone(pheromone, threshold))

  }

  private def createNewAnt(clock:Int, state: EnvironmentInfo, patrollingAntProb: Double): EnvironmentInfo = {
    val antId = state.maxAntId + 1
    val birthPosition = state.anthillInfo.position
    val ant = if(math.random() < patrollingAntProb) {
      context.actorOf(PatrollingAnt(PatrollingAntInfo(
        state.anthill,
        id = antId,
        position = birthPosition,
        time = clock-1), self), s"ant-$antId")
    } else {
      context.actorOf(model.insects.ForagingAnt(ForagingAntInfo(
        state.anthill,
        id = antId,
        position = birthPosition,
        time = clock-1), self), s"f-ant-$antId")
    }
    ant ! Clock(clock)
    state.addAnt(antId, ant)
  }

  private def killInsect(info: InsectInfo, state: EnvironmentInfo): Unit = {
    context.stop(sender)
    val newState = state.removeInsect(info)
    if ((newState.foragingAntsInfo.size + newState.patrollingAntsInfo.size == newState.ants.size)
      && (newState.enemiesInfo.size == newState.enemies.size)) {
      sendInfoToGUI(newState)
    } else {context >>> defaultBehaviour(newState)}
  }

  private def sendInfoToGUI(info: EnvironmentInfo): Unit = {
    val fights = findFights(info.foragingAntsInfo ++ info.patrollingAntsInfo, info.enemiesInfo)
    handleFights(info, fights)
    val obstacles = info.obstacles ++ info.foods
    val insect = info.foragingAntsInfo ++ info.patrollingAntsInfo ++ info.enemiesInfo
    val pheromones: Seq[Pheromone] = info.pheromones
    info.gui.get ! Repaint(info.anthillInfo.get +: (insect ++ obstacles ++ pheromones ++ fights).toSeq)
    context >>> defaultBehaviour(info.emptyInsectInfo())
  }

  private def findFights(antsInfo: Iterable[InsectInfo],
                         enemiesInfo: Iterable[EnemyInfo])
  : Iterable[Fight[InsectInfo, EnemyInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)

  private def handleFights(info: EnvironmentInfo, fights: Iterable[Fight[InsectInfo, EnemyInfo]]): Unit = {
    import model.Fights.InsectFight._
    import model.Fights._
    for (loser <- losers(fights)) {
      loser match {
        case Left(ant) =>
          info.ants(ant) ! KillInsect(ant)
        case Right(enemy) =>
          info.enemies(enemy) ! KillInsect(enemy)
      }
    }
  }

  private implicit def mapToSeqPheromone(map: Map[Int, Pheromone]): Seq[Pheromone] = {
    map.values.toSeq
  }

}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
