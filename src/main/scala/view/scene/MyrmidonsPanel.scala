package view.scene

import java.awt.Color
import java.awt.geom.Ellipse2D

import model.insects.InsectInfo

import scala.swing.{Graphics2D, Panel}

/**
 * Panel that will be contain simulation entities
 * and its view behaviours.
 */
case class MyrmidonsPanel() extends Panel {

  private val antSize = 10
  private val obstacleSize = 50
  private var antsPosition: Seq[InsectInfo] = Seq.empty

  override def paintComponent(g: Graphics2D) {

    g.clearRect(0, 0, size.width, size.height)

    /**
     * Draw first 2 obstacle.
     * In simple-GUI version.
     */
    g.setColor(Color.GRAY)
    g.fillRect(300 - obstacleSize / 2, 300 - obstacleSize / 2, obstacleSize, obstacleSize)
    g.fillRect(600 - obstacleSize / 2, 600 - obstacleSize / 2, obstacleSize, obstacleSize)

    /**
     * Foreach ants draw its new position in Panel.
     * Use java.awt.geom.Ellipse2D because draw with double coordinates.
     */
    g.setColor(Color.black)
    antsPosition.foreach(x => {
      val ellipse = new Ellipse2D.Double(x.position.x * antSize - (antSize / 2),
        x.position.y * antSize - (antSize / 2), antSize, antSize)
      g.fill(ellipse)
    })
  }

  def draw(): Unit = {
    repaint()
  }

  def setAnts(ants: Seq[InsectInfo]): Unit = {
    this.antsPosition = ants
  }
}
