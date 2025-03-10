package model.environment.utility

import akka.actor.{ActorContext, ActorRef}
import common.geometry.Vector2D
import model.environment.data.{EnvironmentInfo, InsectReferences}
import model.environment.elements.{Obstacle, ObstacleFactory}
import model.environment.{MAX_DISTANCE_ENEMIES_FROM_ANTHILL, MIN_DISTANCE_ENEMIES_FROM_ANTHILL}
import model.insects.Ants.ForagingAnt._
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo}
import model.insects.{Enemy, ForagingAnt}

import scala.util.Random

/** Births and deaths function utilities. */
object InsectLifeUtilities {

  /** Stochastic spawn of ant.
   *
   * @param state environment information.
   * @return true if ant has to be spawn
   */
  private[environment] def randomSpawnAnt(state: EnvironmentInfo): Boolean = {
    val antHillFoodPercentage = state.anthillInfo.get.foodAmount / state.anthillInfo.get.maxFoodAmount
    val scaleFactor = ANT_SPAWN_FACTOR / MAX_FOOD
    Random.nextDouble() < (antHillFoodPercentage * scaleFactor)
  }

  /** Stochastic spawn of enemies.
   *
   * @return true if enemy has to be spawn
   */
  private[environment] def randomSpawnEnemies(): Boolean = {
    Random.nextDouble() < ENEMIES_SPAWN_PROBABILITY
  }

  /** Creation new ant.
   *
   * @param clock             current clock
   * @param context           actor context
   * @param state             environment information
   * @param patrollingAntProb probability of spawn patrolling ant
   * @return reference to new ant.
   */
  private[environment] def createNewAnt(clock: Int, context: ActorContext,
                                        state: EnvironmentInfo, patrollingAntProb: Double): ActorRef = {
    val antId = state.maxInsectId._1 + 1
    val birthPosition = state.anthillInfo.position
    if (math.random() < patrollingAntProb) {
      context.actorOf(model.insects.PatrollingAnt(model.insects.info.PatrollingAntInfo(
        state.anthill.get,
        id = antId,
        position = birthPosition,
        time = clock - 1), context.self), s"p-ant-$antId")
    } else {
      context.actorOf(ForagingAnt(ForagingAntInfo(
        state.anthill.get,
        id = antId,
        position = birthPosition,
        time = clock - 1), context.self), s"f-ant-$antId")
    }
  }

  private[environment] def createNewEnemy(clock: Int, context: ActorContext,
                                          state: EnvironmentInfo): ActorRef = {
    val enemyId = state.maxInsectId._2 + 1
    val randomPosition = ObstacleFactory.randomPositionOutObstacleFromCenter(state.obstacles.toSeq,
      state.boundary.center, MIN_DISTANCE_ENEMIES_FROM_ANTHILL, MAX_DISTANCE_ENEMIES_FROM_ANTHILL)
    context.actorOf(Enemy(EnemyInfo(
      id = enemyId,
      position = randomPosition,
      time = clock - 1), context.self), s"enemy-$enemyId")
  }

  /** Create enemies.
   *
   * @param context   actor context
   * @param obstacles list of obstacles
   * @param nEnemies  number of enemies
   * @param center    center of simulation
   * @return references to new enemies.
   */
  private[environment] def createEnemies(context: ActorContext,
                                         obstacles: Seq[Obstacle], nEnemies: Int, center: Vector2D): InsectReferences =
    (0 until nEnemies).map(i => {
      val randomPosition = ObstacleFactory.randomPositionOutObstacleFromCenter(obstacles, center,
        MIN_DISTANCE_ENEMIES_FROM_ANTHILL, MAX_DISTANCE_ENEMIES_FROM_ANTHILL)
      i -> context.actorOf(Enemy(EnemyInfo(id = i, position = randomPosition), context.self), s"enemy-$i")
    }).toMap

  /** Kill insect.
   *
   * @param context actor context
   * @param state   environment state
   * @param info    insect information
   * @return environment state without killed insect.
   */
  private[environment] def killInsect(context: ActorContext,
                                      state: EnvironmentInfo, info: InsectInfo): EnvironmentInfo = {
    context.stop(context.sender())
    state.removeInsect(info)
  }
}
