package view.actor

import akka.actor.{Actor, ActorLogging, Props, Timers}
import model.insects.InsectInfo
import utility.Messages.{Clock, RepaintInsects, UpdateInsect}
import view.scene.{MyrmidonsCanvas, SimulationPane}

import scala.concurrent.duration.DurationInt

/**
 * Gui Actor which can receive following message:
 * - StepOver : the timer is expired and notify environment with Clock message.
 * - updateInsect : from Environment to UiActor to notify
 * new position of the simulation entities.
 */

object UiActor {

  private case object StepKey

  private case object StepOver

  def apply(canvas: MyrmidonsCanvas, pane: SimulationPane): Props =
    Props(classOf[UiActor], canvas, pane)
}

case class UiActor(canvas: MyrmidonsCanvas, pane: SimulationPane)
  extends Actor with ActorLogging with Timers {

  import UiActor._

  var currentState = 1

  override def receive: Receive = defaultBehaviour

  private def defaultBehaviour: Receive = {

    case RepaintInsects(info: Seq[InsectInfo]) =>
      canvas.clear()
      info.foreach(x => canvas.addAnt(x.position.x,x.position.y))
      currentState = currentState + 1
      pane.step.text.value = currentState.toString
      pane.nAnt.text.value = 1.toString
      timers.startSingleTimer(StepKey, StepOver, 17.millis)

    case StepOver =>
      pane.environment.tell(Clock(currentState), self)
  }
}
