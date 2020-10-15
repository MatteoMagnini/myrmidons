package view.actor

import java.io.{FileWriter, PrintWriter}
import java.util

import akka.actor.{Actor, ActorLogging, Props}
import model.Drawable
import model.anthill.AnthillInfo
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}
import utility.RichActor._
import view.actor.uiMessage._
import view.frame.TimeSeriesFrame
import com.google.gson.GsonBuilder
import scala.collection.immutable.ListMap

/**
 * Actor that manage save and report information.
 * @param state ReportManger state.
 */
private[view] class ReportManager(state: ReportManagerInfo) extends Actor with ActorLogging {
  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: ReportManagerInfo): Receive = {

    case ReportInfo(info: Seq[Drawable]) =>
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

      self ! UpdateHistory()
      context >>> defaultBehaviour(state.saveInfo(foragingAnts, patrollingAnts, enemies, anthill))


    case UpdateHistory() =>
      val history = Map(state.currentClock -> (state.foragingAnt.size,
        state.patrollingAnt.size, state.enemies.size, state.anthill.get.foodAmount.toInt))
      context >>> defaultBehaviour(state.updateHistory(history))


    case ShowAndSaveReport() =>
      val historyToFile = new util.ArrayList[util.ArrayList[InfoReport]]()
      val historyElement = new util.ArrayList[InfoReport]()
      /** Order history by clock value  */
      val orderHistory = ListMap(state.history.toSeq.sortBy(_._1): _*)
      orderHistory.foreach { x => historyElement.add(InfoReport(x._1, x._2._1, x._2._2, x._2._3, x._2._4))}
      historyToFile.add(historyElement)
      writeFile(historyToFile)
      val frameTimeSeries = TimeSeriesFrame(state.history)
      frameTimeSeries.visible = true
  }

  /**
   * Write history in json file.
   * The gson library requires the use of util.ArrayList to store collections in a json object.
   * @param list list of list information.
   */
  def writeFile(list: util.ArrayList[util.ArrayList[InfoReport]]): Unit = {
    val writer = new PrintWriter(new FileWriter(REPORT_NAME, false))
    val gson = new GsonBuilder().setPrettyPrinting().create
    list.forEach { x =>writer.write(gson.toJson(x))}
    writer.close()
  }
}

object ReportManager {
  def apply(state: ReportManagerInfo): Props = Props(classOf[ReportManager], state)
}

