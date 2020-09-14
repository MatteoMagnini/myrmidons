package model.environment

import akka.actor.ActorRef
import model.{Bordered, Food}
import model.insects.InsectInfo


/** Internal state of environment. */
trait EnvironmentInfo {

  /** Reference to gui actor */
  def gui:Option[ActorRef]

  /** Boundary constrains of environment */
  def boundary:Boundary

  /** Obstacles in environment */
  def obstacles:Iterable[Bordered]

  /** References to ant actors */
  def ants:Iterable[ActorRef]

  /** Ants information */
  def antsInfo:Iterable[InsectInfo]

  /** Returns updated ants information */
  def updateAntsInfo(antInfo: InsectInfo): EnvironmentInfo

  /** Empties ants information */
  def emptyAntsInfo(): EnvironmentInfo

  def updateFood(food:Food, updatedFood:Food): EnvironmentInfo
}


object EnvironmentInfo {

  def apply(boundary: Boundary): EnvironmentInfo =
    EnvironmentData(None, boundary, Seq.empty, Seq.empty, Seq.empty)

  def apply(gui: Option[ActorRef], boundary: Boundary, obstacles:Seq[Bordered], ants: Seq[ActorRef] ): EnvironmentInfo =
    EnvironmentData(gui, boundary, obstacles, ants, Seq.empty)

  private[this] case class EnvironmentData(override val gui: Option[ActorRef], override val boundary: Boundary,
                                           override val obstacles: Seq[Bordered], override val ants: Seq[ActorRef],
                                           override val antsInfo: Seq[InsectInfo]) extends EnvironmentInfo {

    /** Returns info, adding ant information */
    override def updateAntsInfo(antInfo: InsectInfo): EnvironmentData = this.copy(antsInfo = antInfo +: antsInfo)

    /** Returns info, emptying ants information */
    override def emptyAntsInfo(): EnvironmentData = this.copy(antsInfo = Seq.empty)

    import utility.SeqWithReplace._
    override def updateFood(food: Food, updatedFood:Food): EnvironmentData = this.copy(obstacles = obstacles replace(food, updatedFood))
  }
}
