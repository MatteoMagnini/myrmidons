package view.actor

import akka.actor.{Actor, ActorLogging, Props, Timers}
import model.insects.InsectInfo
import utility.Messages.{Clock, UpdateInsect}
import view.scene.{MyrmidonsCanvas, SimulationPane}

import scala.concurrent.duration.DurationInt

/**
 * Gui Actor which can receive following message:
 * - Step : the timer is expired and a simulation step is start
 * - updateInsect : from Environment to UiActor to notify
 * new position of the simulation entities.
 */

object UiActor {

  private case object StepKey

  private case object FirstStep

  private case object Step

  def apply(canvas: MyrmidonsCanvas, pane: SimulationPane): Props =
    Props(classOf[UiActor], canvas, pane)
}

case class UiActor(canvas: MyrmidonsCanvas, pane: SimulationPane)
  extends Actor
    with ActorLogging with Timers {

  import UiActor._
  // timers.startSingleTimer(StepKey, FirstStep, 500.millis)

  var currentState = 0
  var currPosX = 0.0
  var currPosY = 0.0

  override def receive: Receive = defaultBehaviour

  private def defaultBehaviour: Receive = {
    case UiMessage.FirstStep =>
      timers.startTimerWithFixedDelay(StepKey, Step, 1.second)

    case Step =>
      pane.step.text = (pane.step.text.value.toLong + 1).toString
      currentState = pane.step.text.value.toInt
      pane.environment.tell(Clock(currentState), self)
    case UpdateInsect(info: InsectInfo) => {
      currPosX = info.position.x
      currPosY = info.position.y
      print(info)
    }
    case Clock(value) => {
        canvas.addAnt(currPosX, currPosX)
    }
  }


}
