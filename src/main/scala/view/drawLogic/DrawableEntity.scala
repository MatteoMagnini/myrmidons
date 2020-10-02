package view.drawLogic

import java.awt.{Color, Polygon}
import java.awt.geom.Ellipse2D
import model.Fights.Fight
import model.anthill.AnthillInfo
import model.environment.FoodPheromone
import model.environment.elements.{Food, Obstacle}
import model.insects.info.{EnemyInfo, ForagingAntInfo}
import utility.Parameters.GUIConstant._
import view.ColorUtility.Colors._
import scala.swing.{Dimension, Graphics2D}

trait DrawableEntity[A] {
  def draw(elem: A, g: Graphics2D, size: Dimension)
}

object DrawableEntities {

  def draw[T: DrawableEntity](elem: T, g: Graphics2D, size: Dimension): Unit =
    implicitly[DrawableEntity[T]].draw(elem, g, size)


  def drawEllipse(x: Double, y: Double, w: Double, h: Double, g: Graphics2D): Unit = {
    val ellipse = new Ellipse2D.Double(x, y, w, h)
    g.fill(ellipse)
  }

  implicit object drawPheromone extends DrawableEntity[FoodPheromone] {

    import ImplicitConversion._

    override def draw(elem: FoodPheromone, g: Graphics2D, size: Dimension): Unit = {
      val pheromoneIntensity: Float = elem.intensity / 1000
      g.setColor(PHEROMONE_COLOR(pheromoneIntensity))
      drawEllipse(elem.position.x - (PHEROMONE_SIZE / SET_TO_CENTER),
        size.height - elem.position.y - (PHEROMONE_SIZE / SET_TO_CENTER),
        PHEROMONE_SIZE, PHEROMONE_SIZE, g
      )
    }
  }

  implicit object drawFood extends DrawableEntity[Food] {

    import ImplicitConversion._

    override def draw(elem: Food, g: Graphics2D, size: Dimension): Unit = {
      val foodQuantity: Float = elem.quantity / 1000
      g.setColor(FOOD_COLOR(foodQuantity))
      drawEllipse(elem.position.x - elem.radius, size.height - elem.position.y - elem.radius,
        elem.radius * SET_TO_CENTER, elem.radius * SET_TO_CENTER, g)
    }
  }

  implicit object drawAnt extends DrawableEntity[ForagingAntInfo] {
    override def draw(elem: ForagingAntInfo, g: Graphics2D, size: Dimension): Unit = {
      g.setColor(ANT_COLOR)
      drawEllipse(elem.position.x - (ANT_SIZE / SET_TO_CENTER),
        size.height - elem.position.y - (ANT_SIZE / SET_TO_CENTER),
        ANT_SIZE, ANT_SIZE, g)
    }
  }

  implicit object drawObstacle extends DrawableEntity[Obstacle] {
    override def draw(elem: Obstacle, g: Graphics2D, size: Dimension): Unit = {
      g.setColor(OBSTACLE_COLOR)
      val vertex = for (seg <- elem.segments) yield seg._1
      val xCoordinates = for (v <- vertex) yield Math.round(v.x).toInt
      val yCoordination = for (v <- vertex) yield Math.round(size.height - v.y).toInt
      val p: Polygon = new Polygon(xCoordinates.toArray, yCoordination.toArray, vertex.size)
      g.drawPolygon(p)
      g.fillPolygon(p)
    }
  }

  implicit object drawEnemies extends DrawableEntity[EnemyInfo] {
    override def draw(elem: EnemyInfo, g: Graphics2D, size: Dimension): Unit = {
      g.setColor(ENEMIES_COLOR)
      drawEllipse(elem.position.x - (ANT_SIZE / SET_TO_CENTER),
        size.height - elem.position.y - (ANT_SIZE / SET_TO_CENTER),
        ANT_SIZE, ANT_SIZE, g)
    }
  }

  implicit object drawAnthill extends DrawableEntity[AnthillInfo] {

    import ImplicitConversion._

    override def draw(elem: AnthillInfo, g: Graphics2D, size: Dimension): Unit = {

      val anthillFood: Float = elem.foodAmount / elem.maxFoodAmount
      g.setColor(ANTHILL_COLOR(anthillFood))
      drawEllipse(elem.position.x - elem.radius * SET_TO_CENTER,
        size.height - elem.position.y - elem.radius * SET_TO_CENTER,
        elem.radius * SET_TO_CENTER * SET_TO_CENTER,
        elem.radius * SET_TO_CENTER * SET_TO_CENTER, g)
    }
  }

  implicit object drawFight extends DrawableEntity[Fight[ForagingAntInfo, EnemyInfo]] {
    override def draw(elem: Fight[ForagingAntInfo, EnemyInfo], g: Graphics2D, size: Dimension): Unit = {
      import model.Fights._
      import model.Fights.InsectFight._

      val insectLoser = loser(elem) match {
        case Left(x) => g.setColor(Color.black); x
        case Right(x) => g.setColor(Color.red); x
      }
      drawEllipse(insectLoser.position.x,
        size.height - insectLoser.position.y, FIGHT_SIZE, FIGHT_SIZE, g)
    }
  }


}

object ImplicitConversion {
  implicit def doubleToFloat(value: Double): Float = value.toFloat
}