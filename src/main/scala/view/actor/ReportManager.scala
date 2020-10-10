package view.actor

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import akka.actor.{Actor, ActorContext, ActorLogging, Props}
import model.Drawable
import model.anthill.AnthillInfo
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}
import view.actor.uiMessage.{History, SaveInfo, SaveToFile, ShowReport}
import view.scene.TimeSeriesPanel

private[view] class ReportManager(state: ReportManagerInfo) extends Actor with ActorLogging {
  override def receive: Receive = defaultBehaviour(state)

  initializeFile()

  private def defaultBehaviour(state: ReportManagerInfo): Receive = {


    case SaveInfo(info: Seq[Drawable]) =>

      self ! History(info)
      context >>> defaultBehaviour(state.setState(state.foragingAnt, state.patrollingAnt,
        state.enemies))


    case History(info: Seq[Drawable]) =>

      var foragingAnts: Seq[ForagingAntInfo] = Seq.empty
      var patrollingAnts: Seq[PatrollingAntInfo] = Seq.empty
      var enemies: Seq[EnemyInfo] = Seq.empty
      var anthill: Option[AnthillInfo] = None
      info.foreach {
        case entity: ForagingAntInfo => foragingAnts = entity +: foragingAnts
        case entity: PatrollingAntInfo => patrollingAnts = entity +: patrollingAnts
        case entity: AnthillInfo => anthill = Some(entity)
        case entity: EnemyInfo => enemies = entity +: enemies
        case _ =>
      }

      self ! SaveToFile()
      context >>> defaultBehaviour(state.saveInfo(foragingAnts, patrollingAnts, enemies, anthill))

    case SaveToFile() =>

      writeFile(state.currentClock + "\t\t\t" +
        state.anthill.get.foodAmount.toInt + "\t\t"
        + state.foragingAnt.size + "\t\t\t\t\t" +
        (state.foragingAnt.size - state.currentForagingAntSize).toString +
        "\t\t\t\t" + state.patrollingAnt.size + "\t\t\t\t\t" +
        (state.patrollingAnt.size - state.currentPatrollingAntSize).toString
        + "\t\t\t\t" + (state.enemies.size - state.currentEnemiesAntSize))

      context >>> defaultBehaviour(state.updateHistory(Map(state.currentClock -> (state.foragingAnt.size,
        state.patrollingAnt.size, state.enemies.size))))


    case ShowReport() =>

      val frameTimeSeries = TimeSeriesPanel(state.history)
      frameTimeSeries.visible = true

  }

  /**
   * write a `String` to the `filename`.
   */
  def writeFile(s: String): Unit = {

    import java.io.{FileWriter, PrintWriter}
    val writer = new PrintWriter(new FileWriter("SimulationReport.txt", true))
    writer.write(s + "\n")
    writer.close()
  }

  private def initializeFile(): Unit = {

    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formatDateTime = LocalDateTime.now.format(formatter)
    writeFile("Report Myrmidons - Ant Simulator |" + formatDateTime)
    writeFile("LogicTime|AnthillFood|ForagingAntSize|ForagingAntChange|PatrollingAntSize|PatrollingAntChange|EnemySize")
  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become behaviour
  }

}

object ReportManager {
  def apply(state: ReportManagerInfo): Props = Props(classOf[ReportManager], state)
}