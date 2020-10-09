package model

import model.insects.info.InsectInfo

package object environment {

  val ANTHILL_RADIUS = 15
  val FOOD_AMOUNT = 2000
  val FOOD_RADIUS: (Int, Int) = (100, 150)

  implicit def extractOption[X](value: Option[X]): X = value.get

  implicit def entity2Id[X <: InsectInfo](entity: X): Int = entity.id
}
