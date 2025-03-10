
package view.actor

import model.environment.anthill.AnthillInfo
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}


trait ReportManagerInfo {

  def currentClock: Int

  def foragingAnt: Seq[ForagingAntInfo]

  def patrollingAnt: Seq[PatrollingAntInfo]

  def enemies: Seq[EnemyInfo]

  def history: Seq[InfoReport]

  def anthill: Option[AnthillInfo]

  def currentSize: (Int, Int, Int)

  def updateHistory(update: InfoReport): ReportManagerInfo

  def setState(foraging: Seq[ForagingAntInfo], patrolling: Seq[PatrollingAntInfo],
               enemies: Seq[EnemyInfo]): ReportManagerInfo

  def saveInfo(foragingAnts: Seq[ForagingAntInfo], patrollingAnts: Seq[PatrollingAntInfo],
               enemies: Seq[EnemyInfo], anthill: Option[AnthillInfo]): ReportManagerInfo
}

object ReportManagerInfo {


  def apply(): ReportManagerInfo =
    ReportManagerData(0, Seq.empty, Seq.empty, Seq.empty, Seq.empty, None, (0, 0, 0))

  def apply(currentClock: Int, foragingAnt: Seq[ForagingAntInfo], patrollingAnt: Seq[PatrollingAntInfo],
            enemies: Seq[EnemyInfo], history: Seq[InfoReport],
            anthill: Option[AnthillInfo], currentSize: (Int, Int, Int)): ReportManagerInfo =
    ReportManagerData(currentClock, foragingAnt, patrollingAnt, enemies, history, anthill,
      currentSize)

  private[this] case class ReportManagerData(override val currentClock: Int,
                                             override val foragingAnt: Seq[ForagingAntInfo],
                                             override val patrollingAnt: Seq[PatrollingAntInfo],
                                             override val enemies: Seq[EnemyInfo],
                                             override val history: Seq[InfoReport],
                                             override val anthill: Option[AnthillInfo],
                                             override val currentSize: (Int, Int, Int)
                                            ) extends ReportManagerInfo {


    override def updateHistory(update: InfoReport): ReportManagerInfo =
      this.copy(history = history :+ update)


    override def setState(foraging: Seq[ForagingAntInfo], patrolling: Seq[PatrollingAntInfo],
                          enemies: Seq[EnemyInfo]): ReportManagerInfo = {
      this.copy(
        currentClock = currentClock + REPORT_INC_CLOCK,
        currentSize = (foraging.size, patrolling.size, enemies.size),
        foragingAnt = Seq.empty,
        patrollingAnt = Seq.empty,
        enemies = Seq.empty
      )
    }

    override def saveInfo(foragingAnts: Seq[ForagingAntInfo], patrollingAnts: Seq[PatrollingAntInfo],
                          enemies: Seq[EnemyInfo], anthill: Option[AnthillInfo]): ReportManagerInfo = {
      this.copy(
        foragingAnt = foragingAnts,
        patrollingAnt = patrollingAnts,
        enemies = enemies,
        anthill = anthill
      )
    }
  }
}

