package model.environment

import akka.actor.{Actor, ActorLogging, Props}
import common.Messages._
import common.PheromoneMap._
import common.RichActor._
import common.geometry.{RandomVector2DInCircle, RandomVector2DInSquare, Vector2D, ZeroVector2D}
import model.environment.anthill.{Anthill, AnthillInfo}
import model.environment.data.{EnvironmentInfo, InsectReferences}
import model.environment.elements.EnvironmentElements._
import model.environment.elements.{Food, Obstacle}
import model.environment.pheromones.Pheromone
import model.environment.utility.{CollisionsInterceptor, FightsChecker}
import model.insects.Ants.ForagingAnt._
import model.insects._
import model.insects.info.{SpecificInsectInfo, _}

import scala.util.Random

/** Environment actor
  *
  * @param state environment internal state
  */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = initializationBehaviour(state)

  def getPositionOutObstacle(obstacleList: Seq[Obstacle], min: Double, max: Double): Vector2D = {
    import model.environment.elements.EnvironmentElements.ObstacleHasInside
    var randomPosition = ZeroVector2D()
    do {
      randomPosition = RandomVector2DInCircle(min, max)
    }
    while (checkHaveInside(obstacleList, randomPosition).nonEmpty)
    randomPosition
  }

  def randomPositionOutObstacleFromCenter(obstacleList: Seq[Obstacle],
                                          center: Vector2D,
                                          min: Double, max: Double): Vector2D = {
    import model.environment.elements.EnvironmentElements.ObstacleHasInside
    var randomPosition = ZeroVector2D()
    do {
      randomPosition = common.geometry.RandomVector2DInCircle(min, max, center)
    }
    while (checkHaveInside(obstacleList, randomPosition).nonEmpty)
    randomPosition
  }

  private def initializationBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, nEnemies: Int, obstaclesPresence, foodPresence, anthillFood) =>

      println(state.boundary.center)
      val anthillInfo = AnthillInfo(state.boundary.center, ANTHILL_RADIUS, anthillFood.get)
      val anthill = context.actorOf(Anthill(anthillInfo, self), name = "anthill")
      anthill ! CreateAnts(nAnts, FORAGING_PERCENTAGE)

      val obstacles = if (obstaclesPresence.isDefined) {
        print(anthillInfo.position)
        Obstacle.createRandom(obstaclesPresence.get,
          anthillInfo.position, (50, 150), radius = OBSTACLE_RADIUS).toSeq
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
        val randomPosition = getPositionOutObstacle(obstacles ++ foods,
          MIN_DISTANCE_ENEMIES_FROM_ANTHILL, MAX_DISTANCE_ENEMIES_FROM_ANTHILL)
        i -> context.actorOf(Enemy(EnemyInfo(id = i, position = randomPosition), self), s"enemy-$i")
      }).toMap

      context >>> initializationBehaviour(EnvironmentInfo(Some(sender), state.boundary,
        obstacles, foods, anthill, Some(anthillInfo)).addEnemies(enemies))

    case NewAnts(ants: InsectReferences) =>
      state.gui.get ! Ready
      context >>> defaultBehaviour(state.addAnts(ants))
  }

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case Clock(value: Int) =>
      //println(s"Pheromones: ${state.pheromones.size}, Tree height: ${state.tree.height}")
      randomSpawnAnt(state, value)
      state.ants.values.foreach(_ ! Clock(value))
      state.ants.values.foreach(_ ! Pheromones(state.pheromones, state.tree))
      state.enemies.values.foreach(_ ! Clock(value))
      state.anthill.get ! Clock(value)
      val newData = checkFoodSpawn(state).updatePheromones(state.pheromones.tick())
      //val newData = state.updatePheromones(state.pheromones.tick())
      context >>> defaultBehaviour(newData)

    case Move(position: Vector2D, delta: Vector2D) =>
      CollisionsInterceptor.checkCollisions(sender, state, position, delta)

    case TakeFood(delta, position) =>
      val food = state.foods.filter(f => f.position ~~ (position, 1E-7))
      if (food.nonEmpty) {
        sender ! TakeFood(delta, position)
        val foo = food.head
        val upFoo = foo - delta
        context >>> defaultBehaviour(state.updateFood(foo, upFoo))
      } else {
        sender ! TakeFood(0, position)
      }

    case UpdateInsect(info: SpecificInsectInfo[x]) =>
      val newState = state.updateInsectInfo(info)
      if ((newState.foragingAntsInfo.size + newState.patrollingAntsInfo.size == newState.ants.size)
        && (newState.enemiesInfo.size == newState.enemies.size)) {
        sendInfoToGUI(newState)
      } else {
        context >>> defaultBehaviour(newState)
      }

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context >>> defaultBehaviour(state.updateAnthillInfo(Some(anthillInfo)))

    case AntBirth(clock: Int) => context >>> defaultBehaviour(createNewAnt(clock, state, 0.2f))

    case KillInsect(info: InsectInfo) => killInsect(info, state)

    case AddPheromone(pheromone: Pheromone, threshold: Double) =>
      context >>> defaultBehaviour(state.addPheromone(pheromone, threshold))
  }

  private def checkFoodSpawn(state: EnvironmentInfo): EnvironmentInfo = {

    val envFoodAmount = state.foods.foldRight(0.0)(_.quantity + _)
    val envFoodMeanDistance = state.foods.foldRight(0.0)(_.position --> state.anthillInfo.position + _) / state.foods.size

    val totalFoodOnMeanDistance = envFoodAmount / envFoodMeanDistance
    val antHillFoodPercentage = state.anthillInfo.get.foodAmount / state.anthillInfo.get.maxFoodAmount
    val foodMetricValue = FOOD_METRIC - (antHillFoodPercentage * 10)
    if (totalFoodOnMeanDistance < foodMetricValue) {
      val randomPosition = randomPositionOutObstacleFromCenter(state.obstacles.toList ++ state.foods,
        state.anthillInfo.position, FOOD_RADIUS._1, FOOD_RADIUS._2)
      val nf = Food(randomPosition, FOOD_MIN_QUANTITY)
      state.updateFood(nf, nf)
    } else {
      state
    }
  }

  private def randomSpawnAnt(state: EnvironmentInfo, clock: Int): Unit = {
    val antHillFoodPercentage = state.anthillInfo.get.foodAmount / state.anthillInfo.get.maxFoodAmount
    val scaleFactor = 2.2 / MAX_FOOD
    if (Random.nextDouble() < (antHillFoodPercentage * scaleFactor)) {
      self ! AntBirth(clock)
    }
  }

  private def createNewAnt(clock: Int, state: EnvironmentInfo, patrollingAntProb: Double): EnvironmentInfo = {
    val antId = state.maxAntId + 1
    val birthPosition = state.anthillInfo.position
    val ant = if (math.random() < patrollingAntProb) {
      context.actorOf(PatrollingAnt(PatrollingAntInfo(
        state.anthill,
        id = antId,
        position = birthPosition,
        time = clock - 1), self), s"ant-$antId")
    } else {
      context.actorOf(model.insects.ForagingAnt(ForagingAntInfo(
        state.anthill,
        id = antId,
        position = birthPosition,
        time = clock - 1), self), s"f-ant-$antId")
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
    } else {
      context >>> defaultBehaviour(newState)
    }
  }

  private def sendInfoToGUI(info: EnvironmentInfo): Unit = {
    val fightsChecker = FightsChecker(info.foragingAntsInfo ++ info.patrollingAntsInfo, info.enemiesInfo)
    val fights = fightsChecker.checkFights
    fights._1.foreach(ant => info.ants(ant) ! KillInsect(ant))
    fights._2.foreach(enemy => info.enemies(enemy) ! KillInsect(enemy))

    val obstacles = info.obstacles ++ info.foods
    val insect = info.foragingAntsInfo ++ info.patrollingAntsInfo ++ info.enemiesInfo
    val pheromones: Seq[Pheromone] = info.pheromones
    info.gui.get ! Repaint(info.anthillInfo.get +: (insect ++ obstacles ++ pheromones ++ fightsChecker.fights).toSeq)
    context >>> defaultBehaviour(info.emptyInsectInfo())
  }
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
