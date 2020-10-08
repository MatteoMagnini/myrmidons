package view.scene


import model.Drawable
import model.Fights.Fight
import model.anthill.AnthillInfo
import model.environment.elements.{Food, Obstacle}
import model.environment.pheromones.{DangerPheromone, FoodPheromone}
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}
import view.drawLogic.singletonList
import scala.swing.{Graphics2D, Panel}


/**
 * Panel that will be contain simulation entities
 * and its view behaviours.
 */

private[view] case class MyrmidonsPanel() extends Panel {

  private var restartFlag = false
  private var infoEntities: Seq[Object] = Seq.empty


  override def paintComponent(g: Graphics2D) {

    if (restartFlag) g.clearRect(0, 0, size.width, size.height)
    else {
      g.clearRect(0, 0, size.width, size.height)

      import view.drawLogic.DrawableEntities._

      infoEntities.foreach {

        case entity: ForagingAntInfo => draw(entity, g, size)

        case entity: PatrollingAntInfo => draw(entity, g, size)

        case entity: Food => draw(entity, g, size)

        case entity: Obstacle => draw(entity, g, size)

        case entity: AnthillInfo => draw(entity, g, size)

        case entity: EnemyInfo => draw(entity, g, size)

        case entity: FoodPheromone => draw(entity, g, size)

        case entity: DangerPheromone => draw(entity, g, size)

        case entity: Fight[ForagingAntInfo, EnemyInfo] => draw(entity, g, size)

        case _ => println("Error matching enemies")
      }
      infoEntities = Seq.empty
    }
  }

  def drawEntities(): Unit = {
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

  def setEntities(info: Seq[Drawable]): (Int, Int) = {

    info.foreach(x => infoEntities = singletonList(x).head +: infoEntities)
    val anthillInfo: AnthillInfo = infoEntities.find {
      case _: AnthillInfo => true
      case _ => false
    }.get.asInstanceOf[AnthillInfo]
    val positionsCount = infoEntities.count {
      case _: ForagingAntInfo => true
      case _: PatrollingAntInfo => true
      case _ => false
    }
    (positionsCount, anthillInfo.foodAmount.toInt)
  }

}
