package view.actor

import akka.actor.{Actor, ActorLogging, Props, Timers}
import model.{Food, Placeable, SimpleObstacle}
import model.insects.ForagingAntInfo
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

  private var currentState = 0

  override def receive: Receive = defaultBehaviour

  private def defaultBehaviour: Receive = {

    case Repaint(info: Seq[Placeable]) =>
      var antsInfo:Seq[ForagingAntInfo] = Seq.empty
      var food:Seq[Food] = Seq.empty
      var obstacles:Seq[SimpleObstacle] = Seq.empty
      info.foreach {
        case x:ForagingAntInfo => antsInfo = x +: antsInfo
        case x:Food => food = x +: food
        case x:SimpleObstacle => obstacles = x +: obstacles
        case _ => println("ERROR")
      }
      panel.setAnts(antsInfo)
      panel.setFood(food)
      panel.setObstacles(obstacles)
      panel.draw()
      currentState = currentState + 1
      control.stepText.text = currentState.toString
      control.antPopulationText.text = info.size.toString
      timers.startSingleTimer(currentState, StepOver, 30.millis)

    case StepOver =>
      control.environment.tell(Clock(currentState), self)
  }
}
