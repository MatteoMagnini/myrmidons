package view.actor

import akka.actor.{Actor, ActorLogging, Props, Timers}
import view.scene.{MyrmidonsCanvas, SimulationPane}
import scala.concurrent.duration.DurationInt

/**
 * Gui Actor which can receive following message:
 * - Step : the timer is expired and a simulation step is start
 * - updateInsect : from Environment to UiActor to notify
 * new position of the simulation entities.
 */
object UiActor {

  private case object TickKey

  private case object FirstTick

  private case object Tick

  def apply(canvas: MyrmidonsCanvas, pane: SimulationPane): Props =
    Props(classOf[UiActor], canvas, pane)
}

case class UiActor(canvas: MyrmidonsCanvas, pane: SimulationPane) extends Actor
  with ActorLogging with Timers {

  import UiActor._

  timers.startSingleTimer(TickKey, FirstTick, 500.millis)

  override def receive: Receive = ???

  /*{
    case  updateInsect(info: InsectInfo) =>
  }*/
  /*
  case FirstTick =>
      // do something useful here
      timers.startTimerWithFixedDelay(TickKey, Tick, 1.second)
    case Tick =>
    // do something useful here
    // send clock(value) to enviroment
   */

}
