package view

import view.scene.{ControlPane, LabelPane, MyrmidonsPanel, ParametersPanel}
import utility.Parameters.GUIConstant._

import scala.swing.{BorderPanel, Dimension, MainFrame, Point}

/**
 * Simulation entry point.
 */
object Myrmidons extends App {


  val frame: MainFrame = new MainFrame {
    title = "Myrmidons - Set Parameters"
    val labelPane: ParametersPanel = ParametersPanel()


    contents = new BorderPanel {
      layout += labelPane -> BorderPanel.Position.Center
    }
    size = new Dimension(300 , 300)
    resizable = false
  }
  frame.visible = true

}