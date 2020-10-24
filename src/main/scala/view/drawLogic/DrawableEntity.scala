
package view.drawLogic

import java.awt.geom.Ellipse2D
import java.awt.{Color, Polygon}

import model.environment.Fights.{DeadAnt, DeadEnemy, DeadInsect, Fight}
import model.environment.anthill.AnthillInfo
import model.environment.elements.{Food, Obstacle}
import model.environment.pheromones.{DangerPheromone, DangerPheromoneInfo, Pheromone}
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}
import view.Colors.{ENEMIES_COLOR, _}
import view._

import scala.swing.{Dimension, Graphics2D}

trait DrawableEntity[A] {
  def draw(elem: A, g: Graphics2D, size: Dimension, zoom: Double, slackX: Double, slackY: Double)
}

object DrawableEntities {

  private val center = CENTER
  private val ratio = 1

  /**
   * Draw simulation elem with its parameters.
   * @param elem simulation element.
   * @param graphic graphic.
   * @param size panel size.
   * @tparam T Entities type.
   */
  def draw[T: DrawableEntity](elem: T, graphic: Graphics2D, size: Dimension,
                              zoom: Double, slackX: Double, slackY: Double): Unit =
    implicitly[DrawableEntity[T]].draw(elem, graphic, size, zoom, slackX, slackY)

  /**
   * Draw ellipse in panel.
   * @param x position in x.
   * @param y position in y.
   * @param w weight.
   * @param h height.
   * @param g graphics.
   */
  def drawEllipse(x: Double, y: Double, w: Double, h: Double, g: Graphics2D,
                  zoom: Double, slackX: Double, slackY: Double): Unit = {

    val ellipse = new Ellipse2D.Double(center._1 + x * zoom * ratio + slackX,
      center._2  - y * zoom + slackY, w * zoom, h * zoom)
    g.fill(ellipse)
  }

  implicit object drawPheromone extends DrawableEntity[Pheromone] {
    override def draw(elem: Pheromone, g: Graphics2D, size: Dimension,
                      zoom: Double, slackX: Double, slackY: Double): Unit = {
      val pheromoneIntensity: Float = elem.intensity / DangerPheromoneInfo.MAX_INTENSITY
      g.setColor(elem match {
        case _: DangerPheromone => DANGER_PHEROMONE_COLOR(pheromoneIntensity)
        case _ => FOOD_PHEROMONE_COLOR(pheromoneIntensity)
      })
      drawEllipse(elem.position.x - (PHEROMONE_DRAW_SIZE / SET_TO_CENTER),
         elem.position.y + (PHEROMONE_DRAW_SIZE / SET_TO_CENTER),
        PHEROMONE_DRAW_SIZE, PHEROMONE_DRAW_SIZE, g, zoom, slackX, slackY
      )
    }
  }


  implicit object drawInsect extends DrawableEntity[InsectInfo] {
    override def draw(elem: InsectInfo, g: Graphics2D, size: Dimension,
                      zoom: Double, slackX: Double, slackY: Double): Unit = {
      g.setColor(elem match {
        case _: ForagingAntInfo => ANT_COLOR
        case _: PatrollingAntInfo => PATROLLING_ANT_COLOR
        case _ => ENEMIES_COLOR
      })
      drawEllipse(elem.position.x - (ANT_DRAW_SIZE / SET_TO_CENTER),
        elem.position.y + (ANT_DRAW_SIZE / SET_TO_CENTER),
        ANT_DRAW_SIZE, ANT_DRAW_SIZE, g, zoom, slackX, slackY)
    }
  }

  implicit object drawObstacle extends DrawableEntity[Obstacle] {

    override def draw(elem: Obstacle, g: Graphics2D, size: Dimension,
                      zoom: Double, slackX: Double, slackY: Double): Unit = {
      g.setColor(elem match {
        case food: Food => FOOD_COLOR((food.quantity / 1000).toFloat)
        case _ => OBSTACLE_COLOR
      })
      val vertex = for (seg <- elem.segments) yield seg._1
      val xCoordinates = for (v <- vertex) yield Math.round(center._1 + v.x * zoom * ratio + slackX).toInt
      val yCoordinates = for (v <- vertex) yield Math.round(center._2 - v.y * zoom + slackY).toInt
      val p: Polygon = new Polygon(xCoordinates.toArray, yCoordinates.toArray, vertex.size)
      g.drawPolygon(p)
      g.fillPolygon(p)
    }
  }

  implicit object drawAnthill extends DrawableEntity[AnthillInfo] {
    override def draw(elem: AnthillInfo, g: Graphics2D, size: Dimension,
                      zoom: Double, slackX: Double, slackY: Double): Unit = {
      val anthillFood: Float = elem.foodAmount / elem.maxFoodAmount
      g.setColor(ANTHILL_COLOR(anthillFood))
      drawEllipse(elem.position.x - elem.radius *ANTHILL_SIZE_FACTOR ,
         elem.position.y + elem.radius *ANTHILL_SIZE_FACTOR,
        elem.radius * SET_TO_CENTER * ANTHILL_SIZE_FACTOR ,
        elem.radius * SET_TO_CENTER * ANTHILL_SIZE_FACTOR , g, zoom, slackX, slackY)
    }
  }

  implicit object drawDeadAnt extends DrawableEntity[DeadInsect] {
    override def draw(elem: DeadInsect, g: Graphics2D, size: Dimension,
                      zoom: Double, slackX: Double, slackY: Double): Unit = {
      elem match {
        case x:DeadAnt => g.setColor(Color.black)
        case x:DeadEnemy => g.setColor(Color.red)
      }

      drawEllipse(elem.position.x - FIGHT_DRAW_SIZE / SET_TO_CENTER,
        elem.insect.position.y + FIGHT_DRAW_SIZE / SET_TO_CENTER,
        FIGHT_DRAW_SIZE, FIGHT_DRAW_SIZE, g, zoom, slackX, slackY)
    }
  }

 /* implicit object drawDeadEnemy extends DrawableEntity[DeadEnemy] {
    override def draw(elem: DeadEnemy, g: Graphics2D, size: Dimension,
                      zoom: Double, slackX: Double, slackY: Double): Unit = {
      g.setColor(Color.red)
      drawEllipse(elem.position.x - FIGHT_DRAW_SIZE / SET_TO_CENTER,
        elem.insect.position.y + FIGHT_DRAW_SIZE / SET_TO_CENTER,
        FIGHT_DRAW_SIZE, FIGHT_DRAW_SIZE, g, zoom, slackX, slackY)
    }
  }*/

}
