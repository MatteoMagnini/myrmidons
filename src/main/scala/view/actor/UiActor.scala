package view.actor

import akka.actor.{Actor, ActorLogging, Props, Timers}
import model.insects.InsectInfo
import utility.Messages.UpdateInsect
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

case class UiActor(canvas: MyrmidonsCanvas, pane: SimulationPane) extends Actor
  with ActorLogging with Timers {

  import UiActor._

  timers.startSingleTimer(StepKey, FirstStep, 500.millis)
  var currentState = 0

  override def receive: Receive = {

    case FirstStep =>
      timers.startTimerWithFixedDelay(StepKey, Step, 1.second)

    case Step =>
      currentState = currentState + 1

    case UpdateInsect(info: InsectInfo) =>

  }


}
