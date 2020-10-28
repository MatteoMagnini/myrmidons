package model.environment

import akka.actor.{Actor, ActorLogging, Props}
import common.PheromoneMap._
import common.RichActor._
import common.geometry.Vector2D
import common.message.AnthillMessage.{NewAnts, UpdateAnthill}
import common.message.EnvironmentMessage._
import common.message.InsectMessage._
import common.message.SharedMessage.{Clock, StartSimulation}
import model.environment.anthill.{Anthill, AnthillInfo}
import model.environment.data.{EnvironmentInfo, InsectReferences}
import model.environment.elements.EnvironmentElements.{BoundaryHasInside, _}
import model.environment.elements.{Food, Obstacle, ObstacleFactory}
import model.environment.pheromones.Pheromone
import model.environment.utility.InsectLifeUtilities._
import model.environment.utility.{CollisionsInterceptor, ENEMIES_SPAWN_PROBABILITY, FightsChecker}
import model.insects.info.{SpecificInsectInfo, _}

import scala.util.Random

/** Environment actor
  *
  * @param state environment internal state
  */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = initializationBehaviour(state)

  private def initializationBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, nEnemies: Int, obstaclesPresence, foodPresence, anthillFood) =>

      val anthillInfo = AnthillInfo(state.boundary.center, ANTHILL_RADIUS, anthillFood)
      val anthill = context.actorOf(Anthill(anthillInfo, self), name = "anthill")
      anthill ! CreateAnts(nAnts, FORAGING_PERCENTAGE)

      var obstacles: Seq[Obstacle] = Seq.empty
      var foods: Seq[Food] = Seq.empty
      if (obstaclesPresence.isDefined) {
        obstacles = ObstacleFactory.createRandom(obstaclesPresence,
          anthillInfo.position, MIN_OBSTACLE_RADIUS, MAX_OBSTACLE_RADIUS, radius = OBSTACLE_RADIUS).toSeq
      }
      if (foodPresence.isDefined) {
        foods = (0 until foodPresence).map(_ =>
          Food.createRandomFood(anthillInfo.position, MIN_FOOD_RADIUS, MAX_FOOD_RADIUS))
      }

      val enemies = createEnemies(context, obstacles ++ foods, nEnemies, anthillInfo.position)

      context >>> initializationBehaviour(EnvironmentInfo(Some(sender), state.boundary,
        obstacles, foods, anthill, Some(anthillInfo)).addEnemies(enemies))

    case NewAnts(ants: InsectReferences) =>
      state.gui.get ! Ready
      context >>> defaultBehaviour(state.createStartingAnts(ants))
  }

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {
    case Clock(value: Int) =>
      if (randomSpawnAnt(state)) {
        self ! AntBirth(value)
      }
      if(Random.nextDouble() < ENEMIES_SPAWN_PROBABILITY) self ! EnemyBirth(value)
      state.ants.values.foreach(_ ! Pheromones(state.pheromones, state.tree))
      state.ants.values.foreach(_ ! Clock(value))
      state.enemies.values.foreach(_ ! Clock(value))
      state.anthill.get ! Clock(value)
      val newState = if (value % 100 == 0) checkFoodSpawn(state) else state
      context >>> defaultBehaviour(newState.updatePheromones(newState.pheromones.tick()))

    case Move(position: Vector2D, delta: Vector2D) =>
      if (checkPositionIsInsideObstacle(state.boundary, position >> delta)) {
        CollisionsInterceptor.checkCollisions(sender, state, position, delta)
      } else {
        sender ! NewPosition(position - delta, delta -)
      }

    case TakeFood(delta, position) =>
      val food = state.foods.find(f => f.position ~~ (position, 1E-7))
      if (food.nonEmpty) {
        sender ! TakeFood(delta, position)
        context >>> defaultBehaviour(state.updateFood(food, food - delta))
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

    case AntBirth(clock: Int) =>
      val ant = createNewAnt(clock, context, state, PATROLLING_ANT_PROBABILITY)
      ant ! Clock(clock)
      context >>> defaultBehaviour(state.addAnt(ant))

    case EnemyBirth(clock: Int) =>
      val enemy = createNewEnemy(clock, context, state)
      enemy ! Clock(clock)
      context >>> defaultBehaviour(state.addEnemy(enemy))

    case KillInsect(info: InsectInfo) =>
      val newState = killInsect(context, state, info)
      if ((newState.foragingAntsInfo.size + newState.patrollingAntsInfo.size == newState.ants.size)
        && (newState.enemiesInfo.size == newState.enemies.size)) {
        sendInfoToGUI(newState)
      } else {
        context >>> defaultBehaviour(newState)
      }

    case AddPheromone(pheromone: Pheromone, threshold: Double) =>
      context >>> defaultBehaviour(state.addPheromone(pheromone, threshold))
  }

  private def checkFoodSpawn(state: EnvironmentInfo): EnvironmentInfo = {
    val foodDistance = state.foods.foldRight(0.0)(_.position --> state.anthillInfo.position + _)
    val foodMeanDistance = foodDistance / state.foods.size
    val anthillFoodPercentage = state.anthillInfo.get.foodAmount / state.anthillInfo.get.maxFoodAmount
    val foodMetricValue = FOOD_METRIC - (anthillFoodPercentage * 10)
    if (state.foods.foldRight(0.0)(_.quantity + _) / foodMeanDistance < foodMetricValue) {
      val randomPosition = ObstacleFactory.randomPositionOutObstacleFromCenter(
        state.obstacles.toList ++ state.foods,
        state.anthillInfo.position,
        MIN_FOOD_RADIUS, MAX_FOOD_RADIUS)
      val newFood = Food(randomPosition, FOOD_MIN_QUANTITY)
      state.updateFood(newFood, newFood)
    } else {
      state
    }
  }

  private def sendInfoToGUI(info: EnvironmentInfo): Unit = {
    val fightsChecker = FightsChecker(info.foragingAntsInfo, info.patrollingAntsInfo, info.enemiesInfo)
    val fights = fightsChecker.checkFights
    fights._1.foreach(ant => info.ants(ant.insect) ! KillInsect(ant.insect))
    fights._2.foreach(enemy => info.enemies(enemy.insect) ! KillInsect(enemy.insect))

    val obstacles = info.obstacles ++ info.foods
    val insect = info.foragingAntsInfo ++ info.patrollingAntsInfo ++ info.enemiesInfo
    val pheromones: Seq[Pheromone] = info.pheromones
    info.gui.get ! Repaint(info.anthillInfo.get +: (insect ++ obstacles ++ pheromones ++ fights._1 ++ fights._2).toSeq)
    context >>> defaultBehaviour(info.emptyInsectInfo())
  }
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}
