package view.actor

import model.Drawable
import view.scene.{ControlPane, MyrmidonsPanel}

trait uiActorInfo {

  def setEntities(info: Seq[Drawable]): (Int, Int)

  def rate: Int

  def panel: MyrmidonsPanel

  def control: ControlPane

  def stopFlag: Boolean

  def currentState: Int

  def stopSimulation: uiActorInfo

  def startSimulation: uiActorInfo

  def incCurrentState: uiActorInfo

  def drawEntities(): Unit

  def setControl(currentState: Int, entitiesProperties: (Int, Int)): Unit

  def setRate(rate: Int): uiActorInfo
}

object uiActorInfo {


  def apply(panel: MyrmidonsPanel, control: ControlPane): uiActorInfo =
    uiActorData(panel, control, stopFlag = true, 1, DEFAULT_RATE)

  def apply(panel: MyrmidonsPanel, control: ControlPane, stopFlag: Boolean,
            currentState: Int, rate: Int): uiActorInfo =
    uiActorData(panel, control, stopFlag, currentState, rate)


  private[this] case class uiActorData(override val panel: MyrmidonsPanel,
                                       override val control: ControlPane,
                                       override val stopFlag: Boolean,
                                       override val currentState: Int,
                                       override val rate: Int) extends uiActorInfo {

    override def stopSimulation: uiActorInfo = this.copy(stopFlag = false)

    override def startSimulation: uiActorInfo = this.copy(stopFlag = true)

    override def incCurrentState: uiActorInfo = this.copy(currentState = currentState + 1)

    override def setEntities(info: Seq[Drawable]): (Int, Int) = panel.setEntities(info)


    override def setControl(currentState: Int, entitiesProperties: (Int, Int)): Unit = {
      control.stepText.text = currentState
      control.antPopulationText.text = entitiesProperties._1
      control.anthillFoodAmount.text = entitiesProperties._2
    }

    override def drawEntities(): Unit = panel.drawEntities()

    override def setRate(rate: Int): uiActorInfo = this.copy(rate = rate)
  }

}
