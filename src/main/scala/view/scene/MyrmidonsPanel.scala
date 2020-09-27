package view.scene

import java.awt.Color
import java.awt.geom.{Ellipse2D, Rectangle2D}
import model.anthill.AnthillInfo
import model.environment.FoodPheromone
import model.{Drawable, Food, SimpleObstacle}
import model.insects.{EnemyInfo, ForagingAntInfo, InsectInfo}
import scala.swing.{Graphics2D, Panel}

/**
 * Panel that will be contain simulation entities
 * and its view behaviours.
 */
case class MyrmidonsPanel() extends Panel {

  private val antSize = 4
  private var restartFlag = false

  private var ants: Seq[InsectInfo] = Seq.empty
  private var enemies: Seq[InsectInfo] = Seq.empty
  private var food: Seq[Food] = Seq.empty
  private var anthill: Option[AnthillInfo] = None
  private var obstacles: Seq[SimpleObstacle] = Seq.empty
  private var pheromones: Seq[FoodPheromone] = Seq.empty

  size.height = 800
  size.width = 800

  override def paintComponent(g: Graphics2D) {

    if (restartFlag) g.clearRect(0, 0, size.width, size.height)
    else {

      g.clearRect(0, 0, size.width, size.height)

      /**
       * Foreach pheromones draw its new position in Panel.
       */
      this.pheromones.foreach(x => {
        val d: Float = (x.intensity / 1000).toFloat
        if (d < 0.1f) {
          g.setColor(new Color(1f, 0.8f, 0.02f, 0.1f))
        } else if (d < 0.2f) {
          g.setColor(new Color(1f, 0.6f, 0.04f, 0.2f))
        } else if (d < 0.4f) {
          g.setColor(new Color(1f, 0.4f, 0.06f, 0.4f))
        }
        else if (d < 0.6f) {
          g.setColor(new Color(1f, 0.2f, 0.08f, 0.6f))
        }
        val ellipse = new Ellipse2D.Double(x.position.x - (10 / 2),
          size.height - x.position.y - (10 / 2), 5, 5)
        g.fill(ellipse)
      })

      /**
       * Foreach ants draw its new position in Panel.
       */
      g.setColor(Color.black)
      ants.foreach(x => {
        val ellipse = new Ellipse2D.Double(x.position.x - (antSize / 2),
          size.height - x.position.y - (antSize / 2), antSize, antSize)
        g.fill(ellipse)
      })

      /**
       * Foreach enemies draw its new position in Panel.
       */
      g.setColor(Color.red)
      enemies.foreach(x => {
        val ellipse = new Ellipse2D.Double(x.position.x - (antSize / 2),
          size.height - x.position.y - (antSize / 2), antSize, antSize)
        g.fill(ellipse)
      })

      /**
       * Foreach obstacles draw its new position in Panel.
       */
      g.setColor(new Color(0.5f, 0.5f, 0.5f, 0.5f))
      obstacles.foreach(x => {
        val rect = new Rectangle2D.Double(x.position.x - (x.xDim / 2), size.height - x.position.y - (x.yDim / 2), x.xDim, x.yDim)
        g.fill(rect)
      })

      /**
       * Foreach food resource draw its new position in Panel.
       */
      food.foreach(x => {
        val d: Float = (x.quantity / 1000).toFloat
        if (d < 0.4f) {
          g.setColor(new Color(0f, 0f, 1f, 0.4f))
        } else {
          g.setColor(new Color(0f, 0f, 1f, 0.4f)) //d
        }
        val ellipse = new Ellipse2D.Double(x.position.x - (x.xDim / 2),
          size.height - x.position.y - (x.yDim / 2), x.xDim, x.yDim)
        g.fill(ellipse)

      })

      /**
       * Draw anthill with opacity control.
       */
      if (anthill.nonEmpty) {
        val anthillOpacity: Float = (anthill.get.foodAmount / anthill.get.maxFoodAmount).toFloat
        if (anthillOpacity < 0.4f) {
          g.setColor(new Color(0f, 0.5f, 0f, 0.4f))
        } else {
          g.setColor(new Color(0f, 0.5f, 0f, anthillOpacity))
        }
        val ellipse = new Ellipse2D.Double(anthill.get.position.x - anthill.get.radius * 2,
          size.height - anthill.get.position.y - anthill.get.radius * 2,
          anthill.get.radius * 2 * 2, anthill.get.radius * 2 * 2)
        g.fill(ellipse)
      }


    }
  }

  def draw(): Unit = {
    repaint()
  }

  def clear(): Unit = {
    this.restartFlag = true
  }

  /**
   * Set all new position of entities in next frame.
   *
   * @param info Seq of all the entities that will be draw in panel.
   * @return number of ant.
   */
  def setEntities(info: Seq[Drawable]): Int = {
    ants = Seq.empty
    enemies = Seq.empty
    food = Seq.empty
    obstacles = Seq.empty
    anthill = None
    pheromones = Seq.empty
    info.foreach {
      case x: ForagingAntInfo => ants = x +: ants
      case x: Food => food = x +: food
      case x: SimpleObstacle => obstacles = x +: obstacles
      case x: AnthillInfo => anthill = Some(x)
      case x: EnemyInfo => enemies = x +: enemies
      case x: FoodPheromone => pheromones = x +: pheromones
      case _ => println("Error match entities")
    }
    ants.size
  }

}
