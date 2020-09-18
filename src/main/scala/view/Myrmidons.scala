package view

import akka.actor.Props
import model.{Bordered, SimpleObstacle}
import utility.Geometry.Vector2D
import view.actor.UiActor
import view.scene.{ControlPane, LabelPane, MyrmidonsPanel}

import scala.swing.{BorderPanel, Dimension, MainFrame}

/**
 * Simulation entry point.
 */
object Myrmidons extends App {

  val frame = new MainFrame {
    title = "Myrmidons - Ant Simulator"
    contents = new BorderPanel {
      val myrmidonsPanel: MyrmidonsPanel = MyrmidonsPanel()
      val controlPane: ControlPane = ControlPane(myrmidonsPanel)
      val labelPane: LabelPane = LabelPane()
      layout += controlPane -> BorderPanel.Position.North
      layout += myrmidonsPanel -> BorderPanel.Position.Center
      layout += labelPane -> BorderPanel.Position.South
    }
    size = new Dimension(800, 800)
  }
  frame.visible = true
}