package view.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import model.environment.Environment
import model.insects.InsectInfo
import utility.Messages.{Clock, RepaintInsects}
import view.actor.uiMessage.{RestartSimulation, StopSimulation}
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

  private var currentState = 1
  private var stopFlag = true

  override def receive: Receive = defaultBehaviour

  private def defaultBehaviour: Receive = {

    case RepaintInsects(info: Seq[InsectInfo]) =>
      panel.setAnts(info)
      panel.draw()
      currentState = currentState + 1
      control.stepText.text = currentState.toString
      control.antPopulationText.text = info.size.toString
      if(stopFlag){
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
