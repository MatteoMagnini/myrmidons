package view.scene

import scalafx.Includes._
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.MouseEvent.MouseClicked
import scalafx.scene.paint.Color

/**
 * Canvas that will be contain simulation entities
 * and its view behaviours.
 */
case class MyrmidonsCanvas() extends Canvas {
  private val canvasHeight = 600
  private val canvasWidth = 600
  private val antSize = 10
  private var antsPosition: Set[(Double, Double)] = Set.empty
  private val context = graphicsContext2D

  height = canvasHeight
  width = canvasWidth

  style = "-fx-background-color:#00FF00"
  def initializeCanvas(): Unit = {
    /*Add one ant*/
    context.fill = Color.Black
    context.fillOval(25, 25, 10, 10)
    /* Add one anthill */
    context.fill = Color.Green
    context.fillRect(50, 50, 60, 60)
    /* Add one food resource */
    context.fill = Color.Red
    context.fillRect(120, 120, 20, 20)
  }

  /* When user click in one point of canvas
    a new ant will be filled.
   */
  def addAnt(x: Double, y: Double): Unit = {
    //clear()
    context.fill = Color.Black
    context.fillRect(x * antSize, y * antSize, antSize, antSize)
    antsPosition = antsPosition + ((x, y))

  }
  def clear(): Unit = {
    context.clearRect(0, 0, width.value, height.value)
    antsPosition = Set.empty
  }
  def getAntCount: Int = this.antsPosition.size

  handleEvent(MouseEvent.Any) {
    e: MouseEvent =>
      e.eventType match {
        case MouseClicked =>
          val x = ((e.x - (e.x % antSize)) / antSize).toLong
          val y = ((e.y - (e.y % antSize)) / antSize).toLong
          addAnt(x, y)
        case _ => ()
      }
  }

}
