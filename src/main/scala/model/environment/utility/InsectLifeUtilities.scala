package model.environment.utility

import akka.actor.{ActorContext, ActorRef}
import common.message.SharedMessage.Clock
import model.environment.{MAX_DISTANCE_ENEMIES_FROM_ANTHILL, MIN_DISTANCE_ENEMIES_FROM_ANTHILL}
import model.environment.data.{EnvironmentInfo, InsectReferences}
import model.environment.elements.{Obstacle, ObstacleFactory}
import model.insects.Ants.ForagingAnt._
import model.insects.{Enemy, ForagingAnt}
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo}

import scala.util.Random

object InsectLifeUtilities {
  private[environment] def randomSpawnAnt(state: EnvironmentInfo, clock: Int): Boolean = {
    val antHillFoodPercentage = state.anthillInfo.get.foodAmount / state.anthillInfo.get.maxFoodAmount
    val scaleFactor = 2.2 / MAX_FOOD
    Random.nextDouble() < (antHillFoodPercentage * scaleFactor)
  }

  private[environment] def createNewAnt(clock: Int, context: ActorContext,
                   state: EnvironmentInfo, patrollingAntProb: Double): ActorRef = {
    val antId = state.maxAntId + 1
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

  private[environment] def createEnemies(context: ActorContext,
                                         obstacles: Seq[Obstacle], nEnemies: Int): InsectReferences =
    (0 until nEnemies).map(i => {
    val randomPosition = ObstacleFactory.getPositionOutObstacle(obstacles,
      MIN_DISTANCE_ENEMIES_FROM_ANTHILL, MAX_DISTANCE_ENEMIES_FROM_ANTHILL)
    i -> context.actorOf(Enemy(EnemyInfo(id = i, position = randomPosition), context.self), s"enemy-$i")
  }).toMap

  private[environment] def killInsect(context: ActorContext,
                                      state: EnvironmentInfo, info: InsectInfo): EnvironmentInfo = {
    context.stop(context.sender())
    state.removeInsect(info)
  }
}
