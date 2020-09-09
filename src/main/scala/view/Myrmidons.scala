package view

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{Group, Scene}
import view.scene.SimulationPane

/**
 * Simulation entry point.
 */
object Myrmidons extends JFXApp {
  stage = new PrimaryStage {
    title = "Myrmidons - Ant Simulator"
    private val root = new Group
    root.children = new SimulationPane
    scene = new Scene(root)
  }
}