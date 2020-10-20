package model.environment.data

import akka.actor.ActorRef
import model.environment.anthill.AnthillInfo
import model.environment.Boundary
import model.environment.elements.{Food, Obstacle}
import model.environment.pheromones.Pheromone
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}
import common.PheromoneMap._
import common.rTree.RTree.Tree
import common.rTree.RTreeProlog
import common.rTree.getPheromoneAsNode

/** Internal state of environment. */
sealed trait EnvironmentInfo {

  /** Reference to gui actor */
  def gui: Option[ActorRef]

  /** Boundary constrains of environment */
  def boundary: Boundary

  /** Obstacles in environment */
  def obstacles: Iterable[Obstacle]

  /** Food sources in environment */
  def foods: Iterable[Food]

  /** References to ant actors */
  def ants: InsectReferences

  /** Highest used Id */
  def maxAntId: Int

  /** Ants information */
  def foragingAntsInfo: Iterable[ForagingAntInfo]

  /** Ants information */
  def patrollingAntsInfo: Iterable[PatrollingAntInfo]

  /** References to enemy actors */
  def enemies: InsectReferences

  /** Enemies information */
  def enemiesInfo: Iterable[EnemyInfo]

  /** Anthill information */
  def anthillInfo: Option[AnthillInfo]

  /** Pheromones dropped by ants */
  def pheromones: Map[Int, Pheromone]

  /** RTree for pheromones */
  def tree: Tree[Int]

  /** Prolog engine */
  def engine: RTreeProlog

  /** Reference to anthill */
  def anthill: Option[ActorRef]

  /** Returns updated pheromones */
  def updatePheromones(pheromones: Map[Int, Pheromone]): EnvironmentInfo

  /** Add pheromones */
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

  def addAnts(ants: InsectReferences): EnvironmentInfo

  def addEnemies(ants: InsectReferences): EnvironmentInfo

}

object EnvironmentInfo {

  def apply(boundary: Boundary): EnvironmentInfo =
    EnvironmentData(None, boundary, Seq.empty, Seq.empty,
      Map.empty, 0, Seq.empty, Seq.empty, Map.empty,
      Seq.empty, None, None, Map[Int, Pheromone](), Tree(), RTreeProlog())

  def apply(gui: Option[ActorRef], boundary: Boundary, obstacles: Seq[Obstacle], foods: Seq[Food],
            anthill: ActorRef, anthillInfo: Option[AnthillInfo]): EnvironmentInfo =
    EnvironmentData(gui, boundary, obstacles, foods,
      Map.empty, 0, Seq.empty, Seq.empty, Map.empty,
      Seq.empty, Some(anthill), anthillInfo, Map.empty, Tree(), RTreeProlog())


  /** Internal state of environment.
    *
    * @param gui              reference to gui actor
    * @param boundary         boundary constrains of environment
    * @param obstacles        obstacles in environment
    * @param ants             references to ant actors
    * @param foragingAntsInfo ants information
    * @param anthill          references to anthill actor
    * @param anthillInfo      anthill information
    */
  private[this] case class EnvironmentData(override val gui: Option[ActorRef],
                                           override val boundary: Boundary,
                                           override val obstacles: Seq[Obstacle],
                                           override val foods: Seq[Food],
                                           override val ants: InsectReferences,
                                           override val maxAntId: Int,
                                           override val foragingAntsInfo: Seq[ForagingAntInfo],
                                           override val patrollingAntsInfo: Seq[PatrollingAntInfo],
                                           override val enemies: InsectReferences,
                                           override val enemiesInfo: Seq[EnemyInfo],
                                           override val anthill: Option[ActorRef],
                                           override val anthillInfo: Option[AnthillInfo],
                                           override val pheromones: Map[Int, Pheromone],
                                           override val tree: Tree[Int],
                                           override val engine: RTreeProlog
                                          )
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

    import common.SeqWithReplace._

    override def updateFood(food: Food, updatedFood: Food): EnvironmentData =
      if (updatedFood.quantity > 0) {
        this.copy(foods = (foods filter (f => !(f equals food))) :+ updatedFood)
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

    def addEnemies(newEnemies: InsectReferences): EnvironmentInfo = this.copy(enemies = enemies ++ newEnemies)

    override def updatePheromones(newPheromones: Map[Int, Pheromone]): EnvironmentInfo = {
      val ids = pheromones.keys filterNot newPheromones.keys.toSet
      var newTree = tree
      ids.foreach(id => newTree = engine.removeNode((id, pheromones(id)), newTree))
      this.copy(pheromones = newPheromones, tree = newTree)
    }

    override def addPheromone(pheromone: Pheromone, threshold: Double): EnvironmentInfo = {
      val newMap = pheromones.add(pheromone, threshold)
      val newTree = if (newMap.size > pheromones.size) {
        engine.insertNode((pheromones.nextKey, pheromone), tree)
      } else {
        tree
      }
      this.copy(pheromones = newMap,
        tree = newTree)
    }
  }

}
