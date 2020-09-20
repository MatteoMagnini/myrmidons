package model.environment

import akka.actor.ActorRef
import model.anthill.AnthillInfo
import model.{Bordered, Food}
import model.insects.InsectInfo
import utility.Geometry.ZeroVector2D


/** Internal state of environment. */
trait EnvironmentInfo {

  /** Reference to gui actor */
  def gui: Option[ActorRef]

  /** Boundary constrains of environment */
  def boundary: Boundary

  /** Obstacles in environment */
  def obstacles: Iterable[Bordered]

  /** References to ant actors */
  def ants: Map[Int, ActorRef]

  /** Ants information */
  def antsInfo: Iterable[InsectInfo]

  /** Anthill information */
  def anthillInfo: AnthillInfo

  /** Reference to anthill */
  def anthill: Option[ActorRef]

  /** Returns updated ants information */
  def updateAntsInfo(antInfo: InsectInfo): EnvironmentInfo

  /** Empties ants information */
  def emptyAntsInfo(): EnvironmentInfo

  def updateFood(food: Food, updatedFood: Food): EnvironmentInfo

  def updateAnthillInfo(anthillInfo: AnthillInfo): EnvironmentInfo

  def removeAnt(id: Int): EnvironmentInfo

  //def createAnt(antActorRef: ActorRef, antInfo: InsectInfo): EnvironmentInfo

  //TODO next sprint
  //def removeAnt(antActorRef: ActorRef, antInfo: InsectInfo): EnvironmentInfo
}


object EnvironmentInfo {

  def apply(boundary: Boundary): EnvironmentInfo =
    EnvironmentData(None, boundary, Seq.empty, Map.empty, Seq.empty, None, AnthillInfo(ZeroVector2D()))

  def apply(gui: Option[ActorRef], boundary: Boundary, obstacles: Seq[Bordered],
            ants: Map[Int, ActorRef], anthill: ActorRef, anthillInfo: AnthillInfo): EnvironmentInfo =
    EnvironmentData(gui, boundary, obstacles, ants, Seq.empty, Some(anthill), anthillInfo)

  /** Internal state of environment.
   *
   * @param gui         reference to gui actor
   * @param boundary    boundary constrains of environment
   * @param obstacles   obstacles in environment
   * @param ants        references to ant actors
   * @param antsInfo    ants information
   * @param anthill     references to anthill actor
   * @param anthillInfo anthill information
   */
  private[this] case class EnvironmentData(override val gui: Option[ActorRef], override val boundary: Boundary,
                                           override val obstacles: Seq[Bordered], override val ants: Map[Int, ActorRef],
                                           override val antsInfo: Seq[InsectInfo], override val anthill: Option[ActorRef],
                                           override val anthillInfo: AnthillInfo) extends EnvironmentInfo {

    /** Returns ant info, adding its ActorRef and InsectInfo */
  /*  override def createAnt(antActorRef: ActorRef, antInfo: InsectInfo): EnvironmentInfo = {
      this.copy(ants = antActorRef +: ants, antsInfo = antInfo +: antsInfo)
    }*/

    //TODO next sprint
    /*override def removeAnt(antActorRef: ActorRef, antInfo: InsectInfo): EnvironmentInfo = {
      this.copy(
        ants = this.ants.filter(actorRef => antActorRef != actorRef).seq,
        antsInfo = this.antsInfo.filter(ant => antInfo != ant).seq
      )
    }*/

    /** Returns ant info, adding ant information */
    override def updateAntsInfo(antInfo: InsectInfo): EnvironmentData = this.copy(antsInfo = antInfo +: antsInfo)

    /** Returns ant info, emptying ants information */
    override def emptyAntsInfo(): EnvironmentData = this.copy(antsInfo = Seq.empty)


    import utility.SeqWithReplace._

    override def updateFood(food: Food, updatedFood: Food): EnvironmentData =
      if (updatedFood.quantity > 0) this.copy(obstacles = obstacles replace(food, updatedFood))
      else this.copy(obstacles = obstacles remove food)

    /** Returns  anthill info */
    override def updateAnthillInfo(anthillInfo: AnthillInfo): EnvironmentInfo =
      this.copy(anthillInfo = anthillInfo)

    override def removeAnt(id: Int): EnvironmentInfo = this.copy(ants = ants - id)
  }

}
