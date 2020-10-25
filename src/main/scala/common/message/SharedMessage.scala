package common.message

import akka.actor.ActorContext


object SharedMessage {

  /**
   * Message sent from [[view.actor.UiActor]] to [[model.environment.Environment]]
   * and from [[model.environment.Environment]] to ants, to do a step in simulation.
   *
   * @param value clock value
   */
  case class Clock(value: Int) extends Message

  /** Used only for test purpose.
   *
   * @param context Actor context.
   */
  case class Context(context: Option[ActorContext]) extends Message

  /**
   * Message sent from [[view.actor.UiActor]] to [[model.environment.Environment]], to start simulation.
   *
   * @param nAnts       number of ants.
   * @param nEnemies    number of enemies.
   * @param obstacles   number of obstacle.
   * @param food        number of food sources.
   * @param anthillFood number of food amount in anthill.
   */
  case class StartSimulation(nAnts: Int,
                             nEnemies: Int,
                             obstacles: Option[Int],
                             food: Option[Int],
                             anthillFood: Double) extends Message

}
