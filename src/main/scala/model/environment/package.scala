package model

import model.environment.pheromones.Pheromone
import model.insects.info.InsectInfo

package object environment {

  val ANTHILL_RADIUS = 15
  val FOOD_AMOUNT = 2000
  val MIN_FOOD_RADIUS = 100.00
  val MAX_FOOD_RADIUS = 500.00
  val FORAGING_PERCENTAGE = 0.7

  val FOOD_MIN_QUANTITY = 500
  val FOOD_VERTEX = 16
  val FOOD_MIN_SIZE = 5
  val FOOD_METRIC = 18


  val MIN_OBSTACLE_RADIUS = 100.00
  val MAX_OBSTACLE_RADIUS = 600.00
  val OBSTACLE_TRIANGLE_VERTEX = 3
  val OBSTACLE_SQUARE_VERTEX = 4
  val OBSTACLE_OCTAGON_VERTEX = 8
  val OBSTACLE_RADIUS = 30
  val OBSTACLE_DEFAULT_VERTEX = 10

  val MIN_DISTANCE_ENEMIES_FROM_ANTHILL = 20
  val MAX_DISTANCE_ENEMIES_FROM_ANTHILL = 600

  val PATROLLING_ANT_PROBABILITY = 0.2

  implicit def extractOption[X](value: Option[X]): X = value.get

  implicit def entityToId[X <: InsectInfo](entity: X): Int = entity.id

  implicit def mapToSeqPheromone(map: Map[Int, Pheromone]): Seq[Pheromone] =
    map.values.toSeq

}
