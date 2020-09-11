package view.scene

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.MouseEvent.MouseClicked
import scalafx.scene.paint.Color

/**
 * Canvas that will be contain simulation entities
 * and its view behaviours.
 */
case class MyrmidonsCanvas() extends Canvas {
  private val canvasHeight = 800
  private val canvasWidth = 800
  private val antSize = 10
  private val obstacleSize = 60
  private var antsPosition: Set[(Double, Double)] = Set.empty
  private val context = graphicsContext2D

  height = canvasHeight
  width = canvasWidth

  /* When user click in one point of canvas
    a new ant will be filled.
   */
  def addAnt(x: Double, y: Double): Unit = {
      context.fill = Color.Black
      /* center origin of the ant */
      context.fillOval((x * antSize) - (antSize / 2), (y * antSize) - (antSize / 2), antSize, antSize)
      //antsPosition = antsPosition + ((x, y))
  }

  def clear(): Unit = {
    context.clearRect(0, 0, width.value, height.value)
    context.fill = Color.Brown
    /* center origin of rectange */
    context.fillRect((300 - obstacleSize / 2), (300 - obstacleSize / 2), obstacleSize, obstacleSize)
    /* center origin of rectange */
    context.fillRect((600 - obstacleSize / 2), (600 - obstacleSize / 2), obstacleSize, obstacleSize)
    antsPosition = Set.empty
  }

  def drawObstacle(x: Double, y: Double): Unit ={
    context.fill = Color.Brown
    context.fillRect(x * obstacleSize, y * obstacleSize, obstacleSize, obstacleSize)

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
