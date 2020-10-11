package view.actor

import model.Drawable

private[view] sealed trait uiMessage

object uiMessage {

  case class StopSimulation() extends uiMessage

  case class RestartSimulation() extends uiMessage

  case class StepOver() extends uiMessage

  case class SaveInfo(info: Seq[Drawable]) extends uiMessage

  case class ShowReport() extends uiMessage

  case class History(info: Seq[Drawable]) extends uiMessage

  case class SaveToFile() extends uiMessage

}

