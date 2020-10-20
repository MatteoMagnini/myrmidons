package view



package object drawLogic {

  val ANTHILL_SIZE_FACTOR = 2

  implicit def doubleToFloat(value: Double): Float = value.toFloat

  implicit def doubleToInt(value: Double): Int = value.toInt

}
