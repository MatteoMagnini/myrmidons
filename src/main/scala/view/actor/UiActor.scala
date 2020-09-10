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

  private case object StepOver

  private case object Step

  def apply(canvas: MyrmidonsCanvas, pane: SimulationPane): Props =
    Props(classOf[UiActor], canvas, pane)
}

case class UiActor(canvas: MyrmidonsCanvas, pane: SimulationPane)
  extends Actor
    with ActorLogging with Timers {

  import UiActor._

  var currentState = 1

  override def receive: Receive = defaultBehaviour

  private def defaultBehaviour: Receive = {

    case UpdateInsect(info: InsectInfo) =>
      println("Gui Logic Time: " + info.time)
      canvas.clear()
      canvas.addAnt(info.position.x, info.position.y)
      currentState = currentState + 1
      timers.startSingleTimer(StepKey, StepOver, 17.millis)

    case StepOver =>
      pane.environment.tell(Clock(currentState), self)
  }
}
