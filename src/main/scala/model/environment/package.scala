package model

import model.insects.info.InsectInfo

package object environment {

  val ANTHILL_RADIUS = 15
  val FOOD_AMOUNT = 2000
  val FOOD_RADIUS: (Int, Int) = (120, 200)
  val FORAGING_PERCENTAGE = 0.7

  val FOOD_MIN_QUANTITY = 500
  val FOOD_VERTEX = 16
  val FOOD_MIN_SIZE = 5

  val OBSTACLE_TRIANGLE_VERTEX = 3
  val OBSTACLE_SQUARE_VERTEX = 4
  val OBSTACLE_OCTAGON_VERTEX = 8
  val OBSTACLE_RADIUS = 10


  implicit def extractOption[X](value: Option[X]): X = value.get

  implicit def entity2Id[X <: InsectInfo](entity: X): Int = entity.id

}
