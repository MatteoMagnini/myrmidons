package view.actor

import model.Drawable

private[view] sealed trait uiMessage

object uiMessage {

  /**
   * When user click to stop button the simulation timer is stopped.
   */
  case object StopSimulation extends uiMessage

  /**
   * When user click to restart button the simulation timer resumes.
   */
  case object RestartSimulation extends uiMessage

  /**
   * Auto message when timer is expired. The message Clock to environment is sent.
   */
  case object StepOver extends uiMessage

  /**
   * Every 20 clock information of simulation are store by this message to ReportManager actor.
   *
   * @param info current information of simulation.
   */
  case class ReportInfo(info: Seq[Drawable]) extends uiMessage

  /**
   * When user click report button, information are collect in file and show in plot.
   */
  case object ShowAndSaveReport extends uiMessage

  /**
   * The report manager actor filter information and save size of entities.
   *
   * @param info current history information to save.
   */
  case class History(info: Seq[Drawable]) extends uiMessage

  /**
   * Report manager actor store in history all information until that moment.
   */
  case object UpdateHistory extends uiMessage

  /**
   * When user insert number of rate and click set rate button the timer rate is change.
   *
   * @param rate new timer rate to set.
   */
  case class setRate(rate: Int) extends uiMessage

}

