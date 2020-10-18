package view

import java.awt.Toolkit

import view.scene.SIMULATION_BOUNDARY

package object drawLogic {

  val SIMULATION_SIZE = Toolkit.getDefaultToolkit().getScreenSize
  val CENTER = (SIMULATION_SIZE.width / 2, SIMULATION_SIZE.height / 2)

  implicit def doubleToFloat(value: Double): Float = value.toFloat

  implicit def doubleToInt(value: Double): Int = value.toInt

}
