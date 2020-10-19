package view

import java.awt.Toolkit


package object drawLogic {

  val SIMULATION_SIZE = Toolkit.getDefaultToolkit().getScreenSize
  val CENTER = (SIMULATION_SIZE.width / 2, SIMULATION_SIZE.height / 2)
  val ANTHILL_SIZE_FACTOR = 2

  implicit def doubleToFloat(value: Double): Float = value.toFloat

  implicit def doubleToInt(value: Double): Int = value.toInt

}
