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
    val myrmidonsPanel: MyrmidonsPanel = MyrmidonsPanel()
    val controlPane: ControlPane = ControlPane(myrmidonsPanel)
    val labelPane: LabelPane = LabelPane()
    contents = new BorderPanel {

      layout += controlPane -> BorderPanel.Position.North
      layout += myrmidonsPanel -> BorderPanel.Position.Center
      layout += labelPane -> BorderPanel.Position.South
    }
    size = new Dimension(800 + 10 ,
     800 + controlPane.size.height + labelPane.size.height + 40,
      )
  }
  frame.visible = true
}