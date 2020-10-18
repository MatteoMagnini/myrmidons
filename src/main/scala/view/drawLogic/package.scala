package view


package object drawLogic {

  val SIMULATION_SIZE = (1920,956)
  val CENTER = (1920 / 2, 956 / 2)
  val ANTHILL_SIZE_FACTOR = 2

  implicit def doubleToFloat(value: Double): Float = value.toFloat

  implicit def doubleToInt(value: Double): Int = value.toInt

}
