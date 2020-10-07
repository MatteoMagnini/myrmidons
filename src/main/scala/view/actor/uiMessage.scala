package view.actor


sealed trait uiMessage

object uiMessage {

  case class StopSimulation() extends uiMessage

  case class RestartSimulation() extends uiMessage

  case class StepOver() extends uiMessage


}
