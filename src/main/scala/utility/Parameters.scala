package utility

import utility.Geometry.{Vector2D, ZeroVector2D}

object Parameters {

  object ForagingAntConstant {

    def MAX_ENERGY = 100

    def MAX_FOOD = 10

    def FOOD_ENERGY_CONVERSION = 10

    def STARTING_ENERGY = 100

    def STARTING_TIME = 0

    def STARTING_FOOD_AMOUNT = 0

    def STARTING_POSITION: Vector2D = ZeroVector2D()
  }

}
