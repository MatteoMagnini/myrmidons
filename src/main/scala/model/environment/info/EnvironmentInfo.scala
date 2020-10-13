package model.environment.info

import akka.actor.ActorRef
import model.anthill.AnthillInfo
import model.environment.Boundary
import model.environment.elements.{Food, Obstacle}
import model.environment.pheromones.Pheromone
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}
import utility.PheromoneMap._

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

  /**Highest used Id*/
  def maxAntId: Int

  /** Ants information */
  def foragingAntsInfo: Iterable[ForagingAntInfo]

  def patrollingAntsInfo: Iterable[PatrollingAntInfo]

  /** References to enemy actors */
  def enemies: Map[Int, ActorRef]

  /** Enemies information */
  def enemiesInfo: Iterable[EnemyInfo]

  /** Anthill information */
  def anthillInfo: Option[AnthillInfo]

  /** Pheromones dropped by ants */
  def pheromones: Map[Int, Pheromone]

  /** Reference to anthill */
  def anthill: Option[ActorRef]

  /** Returns updated pheromones*/
  def updatePheromones(pheromones: Map[Int,Pheromone]): EnvironmentInfo

  /** Add pheromones*/
  def addPheromone(pheromone: Pheromone, threshold: Double): EnvironmentInfo

  /** Returns updated insect information */
  def updateInsectInfo(insectInfo: InsectInfo): EnvironmentInfo

  /** Empties ants information */
  def emptyInsectInfo(): EnvironmentInfo

  /** Returns updated food sources */
  def updateFood(food: Food, updatedFood: Food): EnvironmentInfo

  /** Returns updated insect anthill information */
  def updateAnthillInfo(anthillInfo: Option[AnthillInfo]): EnvironmentInfo

  def removeInsect(info: InsectInfo): EnvironmentInfo

  /** Add an ant reference */
  def addAnt(id: Int, ant: ActorRef): EnvironmentInfo

  def addAnts(ants: Map[Int, ActorRef]): EnvironmentInfo

  def addEnemies(ants: Map[Int, ActorRef]): EnvironmentInfo

}

object EnvironmentInfo {

  def apply(boundary: Boundary): EnvironmentInfo =
    EnvironmentData(None, boundary, Seq.empty, Seq.empty,
      Map.empty, 0, Seq.empty, Seq.empty, Map.empty,
      Seq.empty, None, None, Map[Int,Pheromone]())

  def apply(gui: Option[ActorRef], boundary: Boundary, obstacles: Seq[Obstacle], foods: Seq[Food],
            anthill: ActorRef, anthillInfo: Option[AnthillInfo]): EnvironmentInfo =
    EnvironmentData(gui, boundary, obstacles, foods,
      Map.empty, 0, Seq.empty, Seq.empty, Map.empty,
      Seq.empty, Some(anthill), anthillInfo, Map.empty)


  /** Internal state of environment.
   *
   * @param gui         reference to gui actor
   * @param boundary    boundary constrains of environment
   * @param obstacles   obstacles in environment
   * @param ants        references to ant actors
   * @param foragingAntsInfo    ants information
   * @param anthill     references to anthill actor
   * @param anthillInfo anthill information
   */
  private[this] case class EnvironmentData(override val gui: Option[ActorRef],
                                           override val boundary: Boundary,
                                           override val obstacles: Seq[Obstacle],
                                           override val foods: Seq[Food],
                                           override val ants: Map[Int, ActorRef],
                                           override val maxAntId: Int,
                                           override val foragingAntsInfo: Seq[ForagingAntInfo],
                                           override val patrollingAntsInfo: Seq[PatrollingAntInfo],
                                           override val enemies: Map[Int, ActorRef],
                                           override val enemiesInfo: Seq[EnemyInfo],
                                           override val anthill: Option[ActorRef],
                                           override val anthillInfo: Option[AnthillInfo],
                                           override val pheromones: Map[Int,Pheromone])
    extends EnvironmentInfo {

    /** Returns ant info, adding ant information */
    override def updateInsectInfo(insectInfo: InsectInfo): EnvironmentData = insectInfo match {
      case insectInfo: EnemyInfo => this.copy(enemiesInfo = insectInfo +: enemiesInfo)
      case insectInfo: ForagingAntInfo => this.copy(foragingAntsInfo = insectInfo +: foragingAntsInfo)
      case insectInfo: PatrollingAntInfo => this.copy(patrollingAntsInfo = insectInfo +: patrollingAntsInfo)
      case _ => this
    }

    override def emptyInsectInfo(): EnvironmentData =
      this.copy(foragingAntsInfo = Seq.empty, patrollingAntsInfo = Seq.empty, enemiesInfo = Seq.empty)

    import utility.SeqWithReplace._

    override def updateFood(food: Food, updatedFood: Food): EnvironmentData =
      if (updatedFood.quantity > 0) {
        this.copy(foods = (foods filter(f => !(f equals food))) :+ updatedFood)
      } else {
        this.copy(foods = foods remove food)
      }

    override def updateAnthillInfo(anthillInfo: Option[AnthillInfo]): EnvironmentInfo =
      this.copy(anthillInfo = anthillInfo)

    override def removeInsect(info: InsectInfo): EnvironmentInfo = info match {
      case info: ForagingAntInfo => this.copy(ants = ants - info.id)
      case info: PatrollingAntInfo => this.copy(ants = ants - info.id)
      case info: EnemyInfo => this.copy(enemies = enemies - info.id)
      case _ => this
    }

    override def addAnt(id: Int, ant: ActorRef): EnvironmentInfo =
      this.copy(ants = ants + (id -> ant), maxAntId = maxAntId + 1)

    override def addAnts(newAnts: Map[Int, ActorRef]): EnvironmentInfo =
      this.copy(ants = ants ++ newAnts, maxAntId = maxAntId + newAnts.size)

    def addEnemies(newEnemies: Map[Int, ActorRef]): EnvironmentInfo = this.copy(enemies = enemies ++ newEnemies)

    override def updatePheromones(pheromones: Map[Int,Pheromone]): EnvironmentInfo =
      this.copy(pheromones = pheromones)

    override def addPheromone(pheromone: Pheromone, threshold: Double): EnvironmentInfo =
      this.copy(pheromones = pheromones.add(pheromone, threshold))
  }
}
