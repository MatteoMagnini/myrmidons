package view.scene

import akka.actor.{ActorRef, ActorSystem}
import model.environment.info.EnvironmentInfo
import model.environment.{Boundary, Environment}
import utility.Messages.StartSimulation
import view._
import view.actor.uiMessage.{RestartSimulation, ShowReport, StopSimulation, setRate}
import view.actor.{ReportManager, ReportManagerInfo, UiActor, uiActorInfo}

import scala.swing.event.ButtonClicked
import scala.swing.{Button, FlowPanel, GridPanel, Label, Separator, TextField}

/**
 * FlowPanel which contains simulation button.
 *
 * @param myrmidonsPanel Panel when all the entities will be draw.
 */
private[view] case class ControlPane(myrmidonsPanel: MyrmidonsPanel) extends GridPanel(2, 2) {

  private val system = ActorSystem("Myrmidons-system")
  private val boundary = Boundary(0, 0, SIMULATION_BOUNDARY._1, SIMULATION_BOUNDARY._2)
  private val stepLabel = new Label("Step:")
  private val populationLabel = new Label("Ants number:")
  private val anthillFoodAmountLabel = new Label("Anthill Food:")
  private val startButton = new Button("Start")
  private val stopButton = new Button("Stop")
  private val restartButton = new Button("Restart")
  private val reportButton = new Button("Report")
  private val timeLabel = new Label("Clock Rate:")
  private val timeInput = new TextField(columns = 3)
  private val buttonSetTime = new Button("Set Rate")

  var uiActor: ActorRef = _
  var environment: ActorRef = _
  var reportManager: ActorRef = _
  uiActor = system.actorOf(UiActor(uiActorInfo(myrmidonsPanel, this)))
  environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor")
  reportManager = system.actorOf(ReportManager(ReportManagerInfo()))

  var startFlag = false
  var stepText = new Label("0")
  var antPopulationText = new Label("0")
  var anthillFoodAmount = new Label("0")

  this.stopButton.enabled = false
  this.restartButton.enabled = false
  this.reportButton.enabled = false

  private var antSize = 0
  private var anthillFood = 0
  private var obstacleSize = 0
  private var enemiesSize = 0
  private var foodSize = 0

  contents ++= Seq(new FlowPanel {
    contents ++= Seq(startButton, stopButton, restartButton, reportButton,
      timeLabel, timeInput, buttonSetTime)

  }, new FlowPanel {
    contents ++= Seq(stepLabel,
      stepText,
      populationLabel, antPopulationText, new Separator(),
      anthillFoodAmountLabel, anthillFoodAmount)
  })


  listenTo(startButton, stopButton, restartButton, reportButton, buttonSetTime)
  /**
   * When startButton is pressed the UiActor tell to Environment that the simulation
   * can be start with a sequence of obstacles and numbers of ants.
   */

  reactions += {
    case ButtonClicked(component) if component == startButton =>
      this.startButton.enabled = false
      this.stopButton.enabled = true
      this.restartButton.enabled = false
      this.reportButton.enabled = false
      if (!startFlag) {
        tellStart()
      } else {
        uiActor.tell(RestartSimulation(), uiActor)
      }
    case ButtonClicked(component) if component == stopButton =>
      this.startFlag = true
      this.startButton.enabled = true
      this.stopButton.enabled = false
      this.restartButton.enabled = true
      this.reportButton.enabled = true
      uiActor.tell(StopSimulation, uiActor)

    case ButtonClicked(component) if component == restartButton =>
      system.stop(environment)
      system.stop(uiActor)
      system.stop(reportManager)
      this.restartButton.enabled = false
      this.stopButton.enabled = true
      this.startButton.enabled = false
      this.reportButton.enabled = false

      val uiRestart: ActorRef = system.actorOf(UiActor(uiActorInfo(myrmidonsPanel, this)))
      val reportRestart: ActorRef = system.actorOf(ReportManager(ReportManagerInfo()))
      uiActor = uiRestart
      reportManager = reportRestart
      val environmentRestart: ActorRef = system.actorOf(Environment(EnvironmentInfo(boundary)),
        name = s"env+${stepText.text}")
      environment = environmentRestart
      tellStart()

    case ButtonClicked(component) if component == reportButton =>
      reportManager.tell(ShowReport(), uiActor)
    case ButtonClicked(component) if component == buttonSetTime =>
      uiActor ! setRate(timeInput.text.toInt)

  }

  private def tellStart(): Unit = {
    environment.tell(StartSimulation(antSize, enemiesSize, obstacles = Some(obstacleSize),
      food = Some(foodSize), anthillFood = Some(anthillFood)), uiActor)

  }


  def setParameters(antSize: Int, anthillFood: Int, foodSize: Int,
                    obstacleSize: Int, enemiesSize: Int): Unit = {
    this.antSize = antSize
    this.anthillFood = anthillFood
    this.obstacleSize = obstacleSize
    this.enemiesSize = enemiesSize
    this.foodSize = foodSize
  }
}
