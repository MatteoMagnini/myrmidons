package view.scene

import model.Drawable
import model.environment.anthill.AnthillInfo
import model.environment.elements.Obstacle
import model.environment.pheromones.Pheromone
import model.environment.utility.DeadInsect
import model.insects.info.{ForagingAntInfo, InsectInfo, PatrollingAntInfo}
import view.controller.{Keyboard, Mouse, MouseWheel}
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

  /**
    * Show or hide pheromones during simulation.
    *
    * @param value if true hide the pheromones, if false show them.
    */
  def hidePheromones(value: Boolean): Unit

  def zoomIn(): Unit

  def zoomOut(): Unit

  def goNorth(): Unit

  def goEast(): Unit

  def goSouth(): Unit

  def goWest(): Unit

}

object MyrmidonsPanel {

  def apply(): MyrmidonsPanel = new MyrmidonsPanelImpl()

  /** Panel that contains the drawn entities.
    *
    */
  private[view] class MyrmidonsPanelImpl() extends MyrmidonsPanel {

    private var restartFlag = false
    private var infoEntities: Seq[Any] = Seq.empty
    private var showPheromones = true
    private var zoom = 1.0
    private var slackX = 0
    private var slackY = 0

    /** Paint entities of the graphics.
      *
      * @param graphics object for rendering.
      */
    override def paintComponent(graphics: Graphics2D) {

      graphics.clearRect(0, 0, size.width, size.height)
      if (!restartFlag) {

        import view.drawLogic.DrawableEntities._

        infoEntities.foreach {

          case entity: InsectInfo => draw(entity, graphics, size, zoom, slackX, slackY)

          case entity: Obstacle => draw(entity, graphics, size, zoom, slackX, slackY)

          case entity: AnthillInfo => draw(entity, graphics, size, zoom, slackX, slackY)

          case entity: Pheromone if showPheromones => draw(entity, graphics, size, zoom, slackX, slackY)

          case entity: DeadInsect => draw(entity, graphics, size, zoom, slackX, slackY)

          case _ =>
        }
        infoEntities = Seq.empty
      }
    }

    peer.addKeyListener(Keyboard(this))
    peer.addMouseListener(new Mouse(this))
    peer.addMouseWheelListener(new MouseWheel(this))

    def drawEntities(): Unit = {
      repaint()
    }

    def clear(): Unit = {
      this.restartFlag = true
    }

    def setEntities(info: Seq[Drawable]): (Int, Int) = {
      /*
       * For each drawable entity it checks if it has changed since the previous interaction.
       * If not, it does not recreate an object in infoEntities.
       */
      info.foreach(x => infoEntities = singletonSeq(x).head +: infoEntities)

      /*
       * Return info for set anthill food an ants size labels.
       */
      val anthillInfo = infoEntities.find {
        case _: AnthillInfo => true
        case _ => false
      }
      var anthillFoodAmount: Double = 0
        if(anthillInfo.isDefined){
          anthillFoodAmount = anthillInfo.get.asInstanceOf[AnthillInfo].foodAmount
      }
      val antsCount = infoEntities.count {
        case _: ForagingAntInfo => true
        case _: PatrollingAntInfo => true
        case _ => false
      }
      (antsCount, anthillFoodAmount)
    }

    def zoomIn(): Unit = {
      zoom = if (zoom >= MAX_ZOOM) {
        zoom
      } else {
        zoom + ZOOM_STEP
      }
    }

    def zoomOut(): Unit = {
      zoom = if (zoom <= MIN_ZOOM) {
        zoom
      }
      else {
        zoom - ZOOM_STEP
      }
    }

    def goNorth(): Unit = slackY += STEP_LENGTH

    def goEast(): Unit = slackX += STEP_LENGTH

    def goSouth(): Unit = slackY -= STEP_LENGTH

    def goWest(): Unit = slackX -= STEP_LENGTH

    override def hidePheromones(value: Boolean): Unit = showPheromones = !value
  }

}

