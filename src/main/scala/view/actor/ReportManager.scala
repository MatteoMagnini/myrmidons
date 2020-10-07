package view.actor


import akka.actor.{Actor, ActorLogging}
import model.Drawable
import model.Fights.Fight
import model.anthill.AnthillInfo
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}
import view.actor.uiMessage.{SaveInfo, ShowReport}
import view.scene.TimeSeriesPanel
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime


class ReportManager extends Actor with ActorLogging {
  override def receive: Receive = defaultBehaviour()

  var foragingAnt: Seq[ForagingAntInfo] = Seq.empty
  var patrollingAnt: Seq[PatrollingAntInfo] = Seq.empty
  var enemies: Seq[EnemyInfo] = Seq.empty
  var currentForagingAntSize = 0
  var currentPatrollingAntSize = 0
  var currentEnemiesAntSize = 0
  var fights: Seq[Fight[ForagingAntInfo, EnemyInfo]] = Seq.empty
  var anthill: Option[AnthillInfo] = None
  var currentClock = 0
  var fightSizeNumber = 0
  var history: Map[Int, (Int, Int, Int)] = Map.empty

  initializeFile()

  private def defaultBehaviour(): Receive = {

    case SaveInfo(info: Seq[Drawable]) =>

      currentClock = currentClock + 20
      currentForagingAntSize = foragingAnt.size
      currentPatrollingAntSize = patrollingAnt.size
      currentEnemiesAntSize = enemies.size
      enemies = Seq.empty
      foragingAnt = Seq.empty
      patrollingAnt = Seq.empty

      info.foreach {
        case entity: ForagingAntInfo => foragingAnt = entity +: foragingAnt
        case entity: PatrollingAntInfo => patrollingAnt = entity +: patrollingAnt
        case entity: AnthillInfo => anthill = Some(entity)
        case entity: EnemyInfo => enemies = entity +: enemies
        case _ =>
      }
      history += (currentClock -> (foragingAnt.size, patrollingAnt.size, enemies.size))

      writeFile(currentClock + "\t\t\t" + anthill.get.foodAmount.toInt + "\t\t"
        + foragingAnt.size + "\t\t\t\t\t" + (foragingAnt.size - currentForagingAntSize).toString
        + "\t\t\t\t" + patrollingAnt.size + "\t\t\t\t\t" + (patrollingAnt.size - currentPatrollingAntSize).toString
        + "\t\t\t\t" + (enemies.size - currentEnemiesAntSize))

    case ShowReport() =>

      val fr =  TimeSeriesPanel(history)
      fr.visible = true

    case _ =>
  }

  /**
   * write a `String` to the `filename`.
   */
  def writeFile(s: String): Unit = {

    import java.io.FileWriter
    import java.io.PrintWriter
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
}
