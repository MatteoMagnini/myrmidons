package model.environment

import akka.actor.ActorRef
import model.anthill.AnthillInfo
import model.environment.elements.{Food, Obstacle}
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo}
import utility.PheromoneSeq._

/** Internal state of environment. */
trait EnvironmentInfo {

  /** Reference to gui actor */
  def gui: Option[ActorRef]

  /** Boundary constrains of environment */
  def boundary: Boundary

  /** Obstacles in environment */
  def obstacles: Iterable[Obstacle]

  /** Food sources in environment */
  def foods: Iterable[Food]

  /** References to ant actors */
  def ants: Map[Int, ActorRef]

  /** Ants information */
  def antsInfo: Iterable[ForagingAntInfo]

  /** References to enemy actors */
  def enemies: Map[Int, ActorRef]

  /** Enemies information */
  def enemiesInfo: Iterable[EnemyInfo]

  /** Anthill information */
  def anthillInfo: Option[AnthillInfo]

  /** Pheromones dropped by ants */
  def pheromones: Seq[FoodPheromone]

  /** Reference to anthill */
  def anthill: Option[ActorRef]

  /** Returns updated pheromones*/
  def updatePheromones(foodPheromone: Seq[FoodPheromone]): EnvironmentInfo

  /** Add pheromones*/
  def addPheromone(food: FoodPheromone, threshold: Double): EnvironmentInfo

  /** Returns updated insect information */
  def updateInsectInfo(insectInfo: InsectInfo): EnvironmentInfo

  /** Empties ants information */
  def emptyInsectInfo(): EnvironmentInfo

  /** Returns updated food sources */
  def updateFood(food: Food, updatedFood: Food): EnvironmentInfo

  /** Returns updated insect anthill information */
  def updateAnthillInfo(anthillInfo: Option[AnthillInfo]): EnvironmentInfo

  /** Remove an ant reference */
  def removeAnt(id: Int): EnvironmentInfo

  /** Remove an enemy reference */
  def removeEnemy(id: Int): EnvironmentInfo

  /** Add an ant reference */
  def addAnt(id: Int, ant: ActorRef): EnvironmentInfo
}


object EnvironmentInfo {

  def apply(boundary: Boundary): EnvironmentInfo =

    EnvironmentData(None, boundary, Seq.empty, Seq.empty, Map.empty, Seq.empty, Map.empty, Seq.empty, None, None, Seq[FoodPheromone]())

  def apply(gui: Option[ActorRef], boundary: Boundary, obstacles: Seq[Obstacle], foods: Seq[Food], ants: Map[Int, ActorRef],
            enemies: Map[Int, ActorRef], anthill: ActorRef, anthillInfo: Option[AnthillInfo]): EnvironmentInfo =
    EnvironmentData(gui, boundary, obstacles, foods,  ants, Seq.empty, enemies, Seq.empty, Some(anthill), anthillInfo, Seq.empty)


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
                                           override val obstacles: Seq[Obstacle],
                                           override val foods: Seq[Food],
                                           override val ants: Map[Int, ActorRef],
                                           override val antsInfo: Seq[ForagingAntInfo],
                                           override val enemies: Map[Int, ActorRef],
                                           override val enemiesInfo: Seq[EnemyInfo],
                                           override val anthill: Option[ActorRef],
                                           override val anthillInfo: Option[AnthillInfo],
                                           override val pheromones: Seq[FoodPheromone]) extends EnvironmentInfo {

    /** Returns ant info, adding ant information */
    override def updateInsectInfo(insectInfo: InsectInfo): EnvironmentData = insectInfo match {
      case insectInfo: ForagingAntInfo => this.copy(antsInfo = insectInfo +: antsInfo)
      case insectInfo: EnemyInfo => this.copy(enemiesInfo = insectInfo +: enemiesInfo)
      case _ => println("error in updateInsectInfo insect info not recognized"); this
    }

    override def emptyInsectInfo(): EnvironmentData = this.copy(antsInfo = Seq.empty, enemiesInfo = Seq.empty)

    import utility.SeqWithReplace._

    override def updateFood(food: Food, updatedFood: Food): EnvironmentData =
      if (updatedFood.quantity > 0) this.copy(foods = foods replace(food, updatedFood))
      else this.copy(foods = foods remove food)

    override def updateAnthillInfo(anthillInfo: Option[AnthillInfo]): EnvironmentInfo =
      this.copy(anthillInfo = anthillInfo)

    override def removeAnt(id: Int): EnvironmentInfo = this.copy(ants = ants - id)

    override def removeEnemy(id: Int): EnvironmentInfo = this.copy(enemies = enemies - id)

    override def addAnt(id: Int, ant: ActorRef): EnvironmentInfo = this.copy(ants = ants + (id -> ant))

    override def updatePheromones(foodPheromones: Seq[FoodPheromone]): EnvironmentInfo =
      this.copy(pheromones = foodPheromones)

    override def addPheromone(food: FoodPheromone, threshold: Double): EnvironmentInfo =
      this.copy(pheromones = pheromones.add(food, threshold))
  }

}
