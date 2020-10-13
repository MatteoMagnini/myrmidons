package view.actor

import model.Drawable

private[view] sealed trait uiMessage

object uiMessage {

  case class StopSimulation() extends uiMessage

  case class RestartSimulation() extends uiMessage

  case class StepOver() extends uiMessage

  case class ReportInfo(info: Seq[Drawable]) extends uiMessage

  case class ShowAndSaveReport() extends uiMessage

  case class History(info: Seq[Drawable]) extends uiMessage

  case class UpdateHistory() extends uiMessage

  case class setRate(rate: Int) extends uiMessage

}

