package view.actor

import model.Drawable
import view.scene.{ControlPanel, MyrmidonsPanel}

trait UiActorInfo {

  def setDrawableEntities(info: Seq[Drawable]): (Int, Int)

  def rate: Int

  def panel: MyrmidonsPanel

  def control: ControlPanel

  def stopFlag: Boolean

  def currentState: Int

  def stopSimulation: UiActorInfo

  def startSimulation: UiActorInfo

  def incrementCurrentState: UiActorInfo

  def draw(): Unit

  def setControl(currentState: Int, entitiesProperties: (Int, Int)): Unit

  def setRate(rate: Int): UiActorInfo
}

object UiActorInfo {


  def apply(panel: MyrmidonsPanel, control: ControlPanel): UiActorInfo =
    UiActorData(panel, control, stopFlag = true, 1, DEFAULT_RATE)

  def apply(panel: MyrmidonsPanel, control: ControlPanel, stopFlag: Boolean,
            currentState: Int, rate: Int): UiActorInfo =
    UiActorData(panel, control, stopFlag, currentState, rate)


  private[this] case class UiActorData(override val panel: MyrmidonsPanel,
                                       override val control: ControlPanel,
                                       override val stopFlag: Boolean,
                                       override val currentState: Int,
                                       override val rate: Int) extends UiActorInfo {

    override def stopSimulation: UiActorInfo = this.copy(stopFlag = false)

    override def startSimulation: UiActorInfo = this.copy(stopFlag = true)

    override def incrementCurrentState: UiActorInfo = this.copy(currentState = currentState + 1)

    override def setDrawableEntities(info: Seq[Drawable]): (Int, Int) = panel.setEntities(info)

    /** Set in ControlPanel its label which refers simulation info.
     *
     * @param currentState       current state of simulation.
     * @param entitiesProperties current number of entities.
     */
    override def setControl(currentState: Int, entitiesProperties: (Int, Int)): Unit = {
      control.setStepLabel(currentState)
      control.setAntPopulationLabel(entitiesProperties._1)
      control.setAnthillFoodAmount(entitiesProperties._2)
    }

    override def draw(): Unit = panel.drawEntities()

    override def setRate(rate: Int): UiActorInfo = this.copy(rate = rate)
  }

}
