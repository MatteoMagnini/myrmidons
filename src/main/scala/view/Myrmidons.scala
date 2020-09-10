package view

import scalafx.application.{JFXApp, Platform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{Group, Scene}
import scalafx.stage.WindowEvent
import view.scene.SimulationPane
import scalafx.Includes._

/**
 * Simulation entry point.
 */
object Myrmidons extends JFXApp {
  stage = new PrimaryStage {
    resizable = false
    title = "Myrmidons - Ant Simulator"
    private val root = new Group
    root.children = new SimulationPane
    scene = new Scene(root)
    handleEvent(WindowEvent.WindowCloseRequest) {
      _: WindowEvent =>
        Platform.exit()
        System.exit(0)
    }
  }
}