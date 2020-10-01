package view.actor


import view.actor.uiMessage.{RestartSimulation, StopSimulation}
import akka.actor.{Actor, ActorLogging, Props, Timers}
import model.Drawable
import utility.Messages.{Clock, Repaint}
import view.scene.{ControlPane, MyrmidonsPanel}

import scala.concurrent.duration.DurationInt

/**
 * Gui Actor which can receive following message:
 * - StepOver : the timer is expired and notify environment with Clock message.
 * - updateInsect : from Environment to UiActor to notify
 * new position of the simulation entities.
 */

object UiActor {

  private case object StepOver

  def apply(panel: MyrmidonsPanel, control: ControlPane): Props =
    Props(classOf[UiActor], panel, control)
}

case class UiActor(panel: MyrmidonsPanel, control: ControlPane)
  extends Actor with ActorLogging with Timers {

  import UiActor._

  // TODO use context and not variable into actor
  private var stopFlag = true
  private var currentState = 1

  override def receive: Receive = defaultBehaviour

  private def defaultBehaviour: Receive = {

    case Repaint(info: Seq[Drawable]) =>

      val entitiesProperties = panel.setEntities(info)
      panel.draw_()
      currentState = currentState + 1

      import ImplicitConversion._
      control.stepText.text = currentState
      control.antPopulationText.text = entitiesProperties._1
      control.anthillFoodAmount.text = entitiesProperties._2

      if (stopFlag) {
        timers.startSingleTimer(currentState, StepOver, 30.millis)
      }

    case StepOver =>
      control.environment.tell(Clock(currentState), self)

    case StopSimulation(stopFlag: Boolean) =>
      this.stopFlag = stopFlag
      timers.cancel(currentState)

    case RestartSimulation() =>
      this.stopFlag = true
      timers.startSingleTimer(currentState, StepOver, 30.millis)
  }
}

object ImplicitConversion {
  implicit def intToString(value: Int): String = value.toString
}