package view

import view.scene.{ControlPane, InteractionPanel, LabelPane, MyrmidonsPanel}

import scala.swing.{BorderPanel, Dimension, MainFrame}

/**
 * Simulation entry point.
 */
object Myrmidons extends App {

  val frame: MainFrame = new MainFrame {
    title = "Myrmidons - Ant Simulator"
    val myrmidonsPanel: MyrmidonsPanel = MyrmidonsPanel()
    val controlPane: ControlPane = ControlPane(myrmidonsPanel)
    val labelPane: LabelPane = LabelPane()
    val interactionLabel: InteractionPanel = InteractionPanel(controlPane)

    contents = new BorderPanel {
      layout += controlPane -> BorderPanel.Position.North
      layout += myrmidonsPanel -> BorderPanel.Position.Center
      layout += labelPane -> BorderPanel.Position.South
      layout += interactionLabel -> BorderPanel.Position.East
    }
    size = new Dimension(800 + interactionLabel.size.width, 900)
  }
  frame.visible = true
}