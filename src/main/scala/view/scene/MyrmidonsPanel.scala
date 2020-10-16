package view.scene


import model.Drawable
import model.Fights.Fight
import model.environment.pheromones.Pheromone
import model.environment.anthill.AnthillInfo
import model.environment.elements.Obstacle
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}
import view.drawLogic._

import scala.swing.{Graphics2D, Panel}

trait MyrmidonsPanel extends Panel {
  /**
   * Recall paintComponent of scala.swing.
   */
  def drawEntities(): Unit

  /**
   * When restart button is clicked the panel will be cleared.
   */
  def clear(): Unit

  /**
   * Set all new position of entities in next frame.
   *
   * @param info Seq of all the entities that will be draw in panel.
   * @return number of ant and food of anthill.
   */
  def setEntities(info: Seq[Drawable]): (Int, Int)
}

object MyrmidonsPanel {

  def apply(): MyrmidonsPanel = new MyrmidonsPanelImpl()

  /**
   * Panel that contains the drawn entities.
   */
  private[view] class MyrmidonsPanelImpl() extends MyrmidonsPanel {

    private var restartFlag = false
    private var infoEntities: Seq[Any] = Seq.empty

    /**
     * Paint entities of the graphics.
     *
     * @param graphics object for rendering.
     */
    override def paintComponent(graphics: Graphics2D) {

      graphics.clearRect(0, 0, size.width, size.height)
      if (!restartFlag) {

        import view.drawLogic.DrawableEntities._

        infoEntities.foreach {

          case entity: InsectInfo => draw(entity, graphics, size)

          case entity: Obstacle => draw(entity, graphics, size)

          case entity: AnthillInfo => draw(entity, graphics, size)

          case entity: Pheromone => draw(entity, graphics, size)

          case entity: Fight[InsectInfo, EnemyInfo] => draw(entity, graphics, size)

          case _ =>
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

    def setEntities(info: Seq[Drawable]): (Int, Int) = {

      /**
       * For each drawable entity it checks if it has changed since the previous interaction.
       * If not, it does not recreate an object in infoEntities.
       */
      info.foreach(x => infoEntities = singletonSeq(x).head +: infoEntities)

      /**
       * Return info for set anthill food an ants size labels.
       */
      val anthillInfo: AnthillInfo = infoEntities.find {
        case _: AnthillInfo => true
        case _ => false
      }.get.asInstanceOf[AnthillInfo]
      val antsCount = infoEntities.count {
        case _: ForagingAntInfo => true
        case _: PatrollingAntInfo => true
        case _ => false
      }
      (antsCount, anthillInfo.foodAmount)
    }
  }

}

