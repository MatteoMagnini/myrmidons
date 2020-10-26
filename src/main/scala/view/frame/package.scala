package view

import scala.swing.Dimension

package object frame {
  val SETTING_SIZE = 400
  val TIME_SERIES_SIZE: Dimension = SIMULATION_SIZE
  val CHART_SIZE = new Dimension(TIME_SERIES_SIZE.width, TIME_SERIES_SIZE.height / 2 - 25)
}
