package view.scene

import java.awt.Color
import java.awt.geom.{Ellipse2D, Rectangle2D}

import model.Fights.Fight
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

  private val antSize = 5
  private val pheromoneSize = 4
  private var restartFlag = false

  private var ants: Seq[InsectInfo] = Seq.empty
  private var enemies: Seq[InsectInfo] = Seq.empty
  private var food: Seq[Food] = Seq.empty
  private var anthill: Option[AnthillInfo] = None
  private var obstacles: Seq[SimpleObstacle] = Seq.empty
  private var fights: Seq[Fight[InsectInfo]] = Seq.empty
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
        val pheromoneIntensity: Float = (x.intensity / 1000).toFloat
        pheromoneIntensity match {
          case intensity if intensity < 0.2f => g.setColor(new Color(1f, 0.1f, 0.02f, 0.2f))
          case intensity if intensity < 0.4f => g.setColor(new Color(1f, 0.2f, 0.04f, 0.4f))
          case intensity if intensity < 0.6f => g.setColor(new Color(1f, 0.3f, 0.06f, 0.6f))
          case _ => g.setColor(new Color(1f, 0.4f, 0.08f, 1f))
        }
        val ellipse = new Ellipse2D.Double(x.position.x - (pheromoneSize / 2),
          x.position.y - (pheromoneSize / 2), pheromoneSize, pheromoneSize)
        g.fill(ellipse)
      })

      /**
       * Foreach ants draw its new position in Panel.
       */
      g.setColor(Color.black)
      ants.foreach(x => {
        val ellipse = new Ellipse2D.Double(x.position.x - (antSize / 2),
          x.position.y - (antSize / 2), antSize, antSize)
        g.fill(ellipse)
      })

      /**
       * Foreach enemies draw its new position in Panel.
       */
      g.setColor(Color.red)
      enemies.foreach(x => {
        val ellipse = new Ellipse2D.Double(x.position.x - (antSize / 2),
          x.position.y - (antSize / 2), antSize, antSize)
        g.fill(ellipse)
      })

      /**
       * Foreach obstacles draw its new position in Panel.
       */
      g.setColor(new Color(0.5f, 0.5f, 0.5f, 0.5f))
      obstacles.foreach(x => {
        val rect = new Rectangle2D.Double(x.position.x - (x.xDim / 2), x.position.y - (x.yDim / 2), x.xDim, x.yDim)
        g.fill(rect)
      })

      /**
       * Foreach food resource draw its new position in Panel.
       */
      food.foreach(x => {
        val foodQuantity: Float = (x.quantity / 1000).toFloat
        foodQuantity match {
          case quantity if quantity < 0.2f => g.setColor(new Color(0f, 0f, 1f, 0.2f))
          case quantity if quantity < 0.4f => g.setColor(new Color(0f, 0f, 1f, 0.4f))
          case quantity if quantity < 0.6f => g.setColor(new Color(0f, 0f, 1f, 0.6f))
          case quantity if quantity < 0.8f => g.setColor(new Color(0f, 0f, 1f, 0.8f))
          case _ => g.setColor(new Color(0f, 0f, 1f, foodQuantity))
        }
        val ellipse = new Ellipse2D.Double(x.position.x - (x.xDim / 2),
          x.position.y - (x.yDim / 2), x.xDim, x.yDim)
        g.fill(ellipse)

      })

      import model.Fights._
      import model.Fights.InsectFight._

      for((pos, loser) <- fights.map(_.position) zip losers(fights)) {
        loser match {
          case _:ForagingAntInfo => g.setColor(Color.black);print("ant")
          case _ => g.setColor(Color.red);print("ins")
        }
        val ellipse = new Ellipse2D.Double(pos.x,pos.y, 20, 20)
        g.fill(ellipse)
      }

      /**
       * Draw anthill with opacity control.
       */
      if (anthill.nonEmpty) {
        val anthillFood: Float = (anthill.get.foodAmount / anthill.get.maxFoodAmount).toFloat
        anthillFood match {
          case f if f < 0.2f => g.setColor(new Color(0f, 0.5f, 0f, 0.2f))
          case f if f < 0.4f => g.setColor(new Color(0f, 0.5f, 0f, 0.4f))
          case f if f < 0.6f => g.setColor(new Color(0f, 0.5f, 0f, 0.6f))
          case f if f < 0.8f => g.setColor(new Color(0f, 0.5f, 0f, 0.8f))
          case _ => g.setColor(new Color(0f, 0.5f, 0f, anthillFood))
        }

        val ellipse = new Ellipse2D.Double(anthill.get.position.x - anthill.get.radius * 2,
          anthill.get.position.y - anthill.get.radius * 2,
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
  def setEntities(info: Seq[Drawable]): (Int,Int) = {
    ants = Seq.empty
    enemies = Seq.empty
    food = Seq.empty
    obstacles = Seq.empty
    fights = Seq.empty
    pheromones = Seq.empty
    anthill = None

    info.foreach {
      case entity: ForagingAntInfo => ants = entity +: ants
      case entity: Food => food = entity +: food
      case entity: SimpleObstacle => obstacles = entity +: obstacles
      case entity: AnthillInfo => anthill = Some(entity)
      case entity: EnemyInfo => enemies = entity +: enemies
      case entity: FoodPheromone => pheromones = entity +: pheromones
      case entity: Fight[_] => fights = entity.asInstanceOf[Fight[InsectInfo]] +: fights
      case _ => println("Error match entities")
    }
    (ants.size, anthill.get.foodAmount.toInt)
  }

}
