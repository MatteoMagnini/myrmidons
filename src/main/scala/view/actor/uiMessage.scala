package view.actor


sealed trait uiMessage

object uiMessage {

  case class StopSimulation(stopFlag: Boolean) extends uiMessage

  case class RestartSimulation() extends uiMessage

}
