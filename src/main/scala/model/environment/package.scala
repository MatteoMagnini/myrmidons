package model

import model.environment.pheromones.Pheromone
import model.insects.info.InsectInfo

package object environment {

  val ANTHILL_RADIUS = 15
  val FOOD_AMOUNT = 2000
  val FOOD_RADIUS: (Double, Double) = (120, 600)
  val FORAGING_PERCENTAGE = 0.7

  val FOOD_MIN_QUANTITY = 500
  val FOOD_VERTEX = 16
  val FOOD_MIN_SIZE = 5
  val FOOD_METRIC = 18

  val OBSTACLE_RADIUS_MIN_MAX = (50.0, 600.0)
  val OBSTACLE_TRIANGLE_VERTEX = 3
  val OBSTACLE_SQUARE_VERTEX = 4
  val OBSTACLE_OCTAGON_VERTEX = 8
  val OBSTACLE_RADIUS = 30
  val OBSTACLE_DEFAULT_VERTEX = 10

  val MIN_DISTANCE_ENEMIES_FROM_ANTHILL = 20
  val MAX_DISTANCE_ENEMIES_FROM_ANTHILL = 600

  val PATROLLING_ANT_PROBABILITY = 0.2

  implicit def extractOption[X](value: Option[X]): X = value.get

  implicit def entity2Id[X <: InsectInfo](entity: X): Int = entity.id

  implicit def mapToSeqPheromone(map: Map[Int, Pheromone]): Seq[Pheromone] =
    map.values.toSeq

}
