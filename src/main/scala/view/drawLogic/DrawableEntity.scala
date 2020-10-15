
package view.drawLogic

import java.awt.geom.Ellipse2D
import java.awt.{Color, Polygon}

import model.Fights.Fight
import model.environment.anthill.AnthillInfo
import model.environment.elements.{Food, Obstacle}
import model.environment.pheromones.{DangerPheromone, DangerPheromoneInfo, Pheromone}
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}
import view.Colors.{ENEMIES_COLOR, _}
import view._

import scala.swing.{Dimension, Graphics2D}

trait DrawableEntity[A] {
  def draw(elem: A, g: Graphics2D, size: Dimension)
}

object DrawableEntities {

  /**
   * Draw simulation elem with its parameters.
   * @param elem simulation element.
   * @param graphic graphic.
   * @param size panel size.
   * @tparam T Entities type.
   */
  def draw[T: DrawableEntity](elem: T, graphic: Graphics2D, size: Dimension): Unit =
    implicitly[DrawableEntity[T]].draw(elem, graphic, size)

  /**
   * Draw ellipse in panel.
   * @param x position in x.
   * @param y position in y.
   * @param w weight.
   * @param h height.
   * @param g graphics.
   */
  def drawEllipse(x: Double, y: Double, w: Double, h: Double, g: Graphics2D): Unit = {
    val ellipse = new Ellipse2D.Double(x, y, w, h)
    g.fill(ellipse)
  }

  implicit object drawPheromone extends DrawableEntity[Pheromone] {
    override def draw(elem: Pheromone, g: Graphics2D, size: Dimension): Unit = {
      val pheromoneIntensity: Float = elem.intensity / DangerPheromoneInfo.MAX_INTENSITY
      g.setColor(elem match {
        case _: DangerPheromone => DANGER_PHEROMONE_COLOR(pheromoneIntensity)
        case _ => FOOD_PHEROMONE_COLOR(pheromoneIntensity)
      })
      drawEllipse(elem.position.x - (PHEROMONE_DRAW_SIZE / SET_TO_CENTER),
        size.height - elem.position.y - (PHEROMONE_DRAW_SIZE / SET_TO_CENTER),
        PHEROMONE_DRAW_SIZE, PHEROMONE_DRAW_SIZE, g
      )
    }
  }


  implicit object drawInsect extends DrawableEntity[InsectInfo] {
    override def draw(elem: InsectInfo, g: Graphics2D, size: Dimension): Unit = {
      g.setColor(elem match {
        case _: ForagingAntInfo => ANT_COLOR
        case _: PatrollingAntInfo => PATROLLING_ANT_COLOR
        case _ => ENEMIES_COLOR
      })
      drawEllipse(elem.position.x - (ANT_DRAW_SIZE / SET_TO_CENTER),
        size.height - elem.position.y - (ANT_DRAW_SIZE / SET_TO_CENTER),
        ANT_DRAW_SIZE, ANT_DRAW_SIZE, g)
    }
  }

  implicit object drawObstacle extends DrawableEntity[Obstacle] {
    override def draw(elem: Obstacle, g: Graphics2D, size: Dimension): Unit = {
      g.setColor(elem match {
        case food: Food => FOOD_COLOR((food.quantity / 1000).toFloat)
        case _ => OBSTACLE_COLOR
      })
      val vertex = for (seg <- elem.segments) yield seg._1
      val xCoordinates = for (v <- vertex) yield Math.round(v.x).toInt
      val yCoordination = for (v <- vertex) yield Math.round(size.height - v.y).toInt
      val p: Polygon = new Polygon(xCoordinates.toArray, yCoordination.toArray, vertex.size)
      g.drawPolygon(p)
      g.fillPolygon(p)
    }
  }

  implicit object drawAnthill extends DrawableEntity[AnthillInfo] {
    override def draw(elem: AnthillInfo, g: Graphics2D, size: Dimension): Unit = {
      val anthillFood: Float = elem.foodAmount / elem.maxFoodAmount
      g.setColor(ANTHILL_COLOR(anthillFood))
      drawEllipse(elem.position.x - elem.radius * SET_TO_CENTER,
        size.height - elem.position.y - elem.radius * SET_TO_CENTER,
        elem.radius * SET_TO_CENTER * SET_TO_CENTER,
        elem.radius * SET_TO_CENTER * SET_TO_CENTER, g)
    }
  }

  implicit object drawFight extends DrawableEntity[Fight[InsectInfo, EnemyInfo]] {
    override def draw(elem: Fight[InsectInfo, EnemyInfo], g: Graphics2D, size: Dimension): Unit = {
      import model.Fights.InsectFight._
      import model.Fights._
      val insectLoser = loser(elem) match {
        case Left(x) => g.setColor(Color.black); x
        case Right(x) => g.setColor(Color.red); x
      }
      drawEllipse(insectLoser.position.x,
        size.height - insectLoser.position.y, FIGHT_DRAW_SIZE, FIGHT_DRAW_SIZE, g)
    }
  }

}
