package view.scene

import java.awt.{Color, Graphics2D}
import java.awt.geom.Ellipse2D

import model.insects.InsectInfo
import utility.Geometry.Vector2D

import scala.swing.event.MouseClicked
import scala.swing.{Graphics2D, Panel}

/**
 * Panel that will be contain simulation entities
 * and its view behaviours.
 */
case class MyrmidonsPanel() extends Panel {

  private val antSize = 10
  private val obstacleSize = 50
  private var antsPosition: Seq[InsectInfo] = Seq.empty
  private var restartFlag = false
  private var drawAnts : Seq[(Int, Int)] = Seq.empty


  override def paintComponent(g: Graphics2D) {

    if (restartFlag) {
      g.clearRect(0, 0, size.width, size.height)
    } else {
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
      drawAnts.foreach(x => {
        val ellipse = new Ellipse2D.Double(x._1/10 * antSize - (antSize / 2),
          x._2/10 * antSize - (antSize / 2), antSize, antSize)
        g.fill(ellipse)
      })
    }
  }

  listenTo(mouse.clicks)
  reactions += {
    case e: MouseClicked =>
      print(e.point.x)
      this.drawAnts = Tuple2(e.point.x, e.point.y) +: this.drawAnts
  }

  def draw(): Unit = {
    repaint()
  }

  def clear(): Unit = {
    this.restartFlag = true
  }

  def setAnts(ants: Seq[InsectInfo]): Unit = {
    this.antsPosition = ants
  }
}
