package view

import java.awt.Toolkit

import view.scene.SIMULATION_BOUNDARY

package object frame {
  val SETTING_SIZE = 400
  val SIMULATION_SIZE = Toolkit.getDefaultToolkit().getScreenSize
  val CENTER_MYR = (SIMULATION_SIZE.width / 2, SIMULATION_SIZE.height / 2)
}
