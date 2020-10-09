package view.actor

import model.anthill.AnthillInfo
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}


trait ReportManagerInfo {

  def currentClock: Int

  def foragingAnt: Seq[ForagingAntInfo]

  def patrollingAnt: Seq[PatrollingAntInfo]

  def enemies: Seq[EnemyInfo]

  def history: Map[Int, (Int, Int, Int)]

  def anthill: Option[AnthillInfo]

  def currentForagingAntSize: Int

  def currentPatrollingAntSize: Int

  def currentEnemiesAntSize: Int

  def updateHistory(update: Map[Int, (Int, Int, Int)]): ReportManagerInfo

  def setState(foraging: Seq[ForagingAntInfo], patrolling: Seq[PatrollingAntInfo], enemies: Seq[EnemyInfo]): ReportManagerInfo

  def saveInfo(foragingAnts: Seq[ForagingAntInfo], patrollingAnts: Seq[PatrollingAntInfo],
               enemies: Seq[EnemyInfo], anthill: Option[AnthillInfo]): ReportManagerInfo
}

object ReportManagerInfo {


  def apply(): ReportManagerInfo =
    ReportManagerData(0, Seq.empty, Seq.empty, Seq.empty, Map.empty, None, 0, 0, 0)

  def apply(currentClock: Int, foragingAnt: Seq[ForagingAntInfo], patrollingAnt: Seq[PatrollingAntInfo],
            enemies: Seq[EnemyInfo], history: Map[Int, (Int, Int, Int)],
            anthill: Option[AnthillInfo], currentForagingAntSize: Int,
            currentPatrollingAntSize: Int, currentEnemiesAntSize: Int): ReportManagerInfo =
    ReportManagerData(currentClock, foragingAnt, patrollingAnt, enemies, history, anthill,
      currentForagingAntSize, currentPatrollingAntSize, currentEnemiesAntSize)

  private[this] case class ReportManagerData(override val currentClock: Int,
                                             override val foragingAnt: Seq[ForagingAntInfo],
                                             override val patrollingAnt: Seq[PatrollingAntInfo],
                                             override val enemies: Seq[EnemyInfo],
                                             override val history: Map[Int, (Int, Int, Int)],
                                             override val anthill: Option[AnthillInfo],
                                             override val currentForagingAntSize: Int,
                                             override val currentPatrollingAntSize: Int,
                                             override val currentEnemiesAntSize: Int
                                            ) extends ReportManagerInfo {


    override def updateHistory(update: Map[Int, (Int, Int, Int)]): ReportManagerInfo =
      this.copy(history = history ++ update)


    override def setState(foraging: Seq[ForagingAntInfo], patrolling: Seq[PatrollingAntInfo], enemies: Seq[EnemyInfo]): ReportManagerInfo = {
      this.copy(
        currentClock = currentClock + 20,
        currentForagingAntSize = foraging.size,
        currentPatrollingAntSize = patrolling.size,
        currentEnemiesAntSize = enemies.size,
        foragingAnt = Seq.empty,
        patrollingAnt = Seq.empty,
        enemies = Seq.empty
      )
    }

    override def saveInfo(foragingAnts: Seq[ForagingAntInfo], patrollingAnts: Seq[PatrollingAntInfo], enemies: Seq[EnemyInfo], anthill: Option[AnthillInfo]): ReportManagerInfo = {
      this.copy(
        foragingAnt = foragingAnts,
        patrollingAnt = patrollingAnts,
        enemies = enemies,
        anthill = anthill
      )
    }
  }

}