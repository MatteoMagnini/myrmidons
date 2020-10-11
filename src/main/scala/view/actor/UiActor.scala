package view.actor

import akka.actor.{Actor, ActorContext, ActorLogging, Props, Timers}
import model.Drawable
import utility.Messages.{Clock, Ready, Repaint}
import view.actor.uiMessage.{RestartSimulation, SaveInfo, StepOver, StopSimulation}

import scala.concurrent.duration.DurationInt

/**
 * Gui Actor which can receive following message:
 * - StepOver : the timer is expired and notify environment with Clock message.
 * - updateInsect : from Environment to UiActor to notify
 * new position of the simulation entities.
 */

private[view] class UiActor(state: uiActorInfo)
  extends Actor with ActorLogging with Timers {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: uiActorInfo): Receive = {

    case Ready => timers.startSingleTimer(state.currentState, StepOver, 30.millis)

    case Repaint(info: Seq[Drawable]) =>
      if (state.currentState % 20 == 0) state.control.reportManager.tell(SaveInfo(info), self)
      val entitiesProperties = state.setEntities(info)
      state.drawEntities()
      state.setControl(state.currentState, entitiesProperties)

      if (state.stopFlag) {
        timers.startSingleTimer(state.currentState, StepOver, 30.millis)
      }
      context >>> defaultBehaviour(state.incCurrentState)

    case StepOver =>
      state.control.environment.tell(Clock(state.currentState), self)
      context >>> defaultBehaviour(uiActorInfo(state.panel, state.control,
        state.stopFlag, state.currentState))

    case StopSimulation =>
      timers.cancel(state.currentState)
      context >>> defaultBehaviour(state.stopSimulation)

    case RestartSimulation() =>
      timers.startSingleTimer(state.currentState, StepOver, 30.millis)
      context >>> defaultBehaviour(state.startSimulation)
  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become behaviour
  }

}

object UiActor {
  def apply(state: uiActorInfo): Props = Props(classOf[UiActor], state)
}