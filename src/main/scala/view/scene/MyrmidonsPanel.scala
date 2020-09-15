package view.scene

import java.awt.Color
import java.awt.geom.{Ellipse2D, Rectangle2D}

import model.{Food, SimpleObstacle}
import model.insects.InsectInfo

import scala.swing.{Graphics2D, Panel}

/**
 * Panel that will be contain simulation entities
 * and its view behaviours.
 */
case class MyrmidonsPanel() extends Panel {

  private val antSize = 10
  private var antsPosition: Seq[InsectInfo] = Seq.empty
  private var food: Seq[Food] = Seq.empty
  private var obstacles: Seq[SimpleObstacle] = Seq.empty

  override def paintComponent(g: Graphics2D) {

    g.clearRect(0, 0, size.width, size.height)

    /**
     * Draw first 2 obstacle.
     * In simple-GUI version.
     */
    g.setColor(Color.gray)

    obstacles.foreach(x => {
      val rect = new Rectangle2D.Double(x.position.x - (x.xDim / 2), x.position.y - (x.yDim / 2), x.xDim, x.yDim)
      g.fill(rect)
    })

    /**
     * Foreach ants draw its new position in Panel.
     * Use java.awt.geom.Ellipse2D because draw with double coordinates.
     */
    g.setColor(Color.black)
    antsPosition.foreach(x => {
      val ellipse = new Ellipse2D.Double(x.position.x - (antSize / 2),
        x.position.y - (antSize / 2), antSize, antSize)
      g.fill(ellipse)
    })

    g.setColor(Color.red)
    food.foreach(x => {
      val ellipse = new Ellipse2D.Double(x.position.x - (x.xDim / 2),
        x.position.y - (x.yDim / 2), x.xDim, x.yDim)
      g.fill(ellipse)
    })

  }

  def draw(): Unit = {
    repaint()
  }

  def setAnts(ants: Seq[InsectInfo]): Unit = {
    this.antsPosition = ants
  }

  def setFood(food: Seq[Food]): Unit = {
    this.food = food
  }

  def setObstacles(obstacles: Seq[SimpleObstacle]): Unit = {
    this.obstacles = obstacles
  }
}
