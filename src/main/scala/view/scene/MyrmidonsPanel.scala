package view.scene

import java.awt.Color

import model.insects.InsectInfo

import scala.swing.{Graphics2D, Panel}

/**
 * Panel that will be contain simulation entities
 * and its view behaviours.
 */
case class MyrmidonsPanel() extends Panel {

  private val antSize = 10
  private val obstacleSize = 60
  private var antsPosition: Seq[InsectInfo] = Seq.empty

  override def paintComponent(g: Graphics2D) {

    g.clearRect(0, 0, size.width, size.height)

    /**
     * Draw first 2 obstacle.
     */
    g.setColor(Color.GRAY)
    g.fillRect((300 - obstacleSize / 2), (300 - obstacleSize / 2), obstacleSize, obstacleSize)
    g.fillRect((600 - obstacleSize / 2), (600 - obstacleSize / 2), obstacleSize, obstacleSize)

    /**
     * Foreach ants draw its new position in Panel.
     */
    g.setColor(Color.black)
    antsPosition.foreach(x => {
      g.fillOval((x.position.x.toInt * antSize) - (antSize / 2),
        (x.position.y.toInt * antSize) - (antSize / 2), antSize, antSize)
    })
  }

  def draw(): Unit = {
    repaint()
  }

  def setAnts(ants: Seq[InsectInfo]): Unit = {
    this.antsPosition = ants
  }
}
