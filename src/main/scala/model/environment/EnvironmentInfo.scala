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
    EnvironmentData(None, boundary, Seq.empty, Seq.empty, Seq.empty, None)

  def apply(gui: Option[ActorRef], boundary: Boundary, obstacles:Seq[Bordered], ants: Seq[ActorRef], anthill: Option[ActorRef] = None ): EnvironmentInfo =
    EnvironmentData(gui, boundary, obstacles, ants, Seq.empty, anthill)

  /** Internal state of environment.
    *
    * @param gui reference to gui actor
    * @param boundary boundary constrains of environment
    * @param obstacles obstacles in environment
    * @param ants references to ant actors
    * @param antsInfo ants information
    * @param anthill ants anthill reference
    */
  private[this] case class EnvironmentData(override val gui: Option[ActorRef], override val boundary: Boundary,
                                           override val obstacles: Seq[Bordered], override val ants: Seq[ActorRef],
                                           override val antsInfo: Seq[InsectInfo], anthill: Option[ActorRef]) extends EnvironmentInfo {

    /** Returns info, adding ant information */
    override def updateAntsInfo(antInfo: InsectInfo): EnvironmentData = this.copy(antsInfo = antInfo +: antsInfo)

    /** Returns info, emptying ants information */
    override def emptyAntsInfo(): EnvironmentData = this.copy(antsInfo = Seq.empty)

    import utility.SeqWithReplace._
    override def updateFood(food: Food, updatedFood: Food): EnvironmentData =
      if (updatedFood.quantity > 0) this.copy(obstacles = obstacles replace(food, updatedFood))
      else this.copy(obstacles = obstacles remove food)
  }
}
