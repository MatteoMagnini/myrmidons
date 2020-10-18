package view.scene

import akka.actor.{ActorRef, ActorSystem}
import model.environment.data.EnvironmentInfo
import model.environment.{Boundary, Environment}
import common.Messages.StartSimulation
import view.actor.uiMessage.{RestartSimulation, ShowAndSaveReport, StopSimulation, setRate}
import view.actor.{ReportManager, ReportManagerInfo, UiActor, uiActorInfo}
import scala.swing.event.ButtonClicked
import scala.swing.{Button, FlowPanel, GridPanel, Label, Separator, TextField}

trait ControlPanel extends GridPanel {

  def myrmidonsPanel: MyrmidonsPanel

  /** Set simulation parameters */
  def setParameters(antSize: Int, anthillFood: Int, foodSize: Int,
                    obstacleSize: Int, enemiesSize: Int): Unit

  /** ReportManager actor ref */
  def reportManager: ActorRef

  /** Environment actor ref */
  def environment: ActorRef

  /** Flag to control timer */
  def startFlag: Boolean

  /** Label, use to set each step in simulation */
  def stepText: Label

  /** Label, use to set each ant size in simulation */
  def antPopulationText: Label

  /** Label, use to set each anthill food in simulation */
  def anthillFoodAmount: Label
}

object ControlPanel {
  def apply(myrmidonsPanel: MyrmidonsPanel): ControlPanel = new ControlPanelImpl(myrmidonsPanel)

  /**
   * GridPane with simulation control button.
   *
   * @param myrmidonsPanel panel to pass at actor to draw entities.
   */
  private[view] class ControlPanelImpl(override val myrmidonsPanel: MyrmidonsPanel)
    extends GridPanel(2, 2) with ControlPanel {

    private val system = ActorSystem("Myrmidons-system")
    private val boundary = Boundary(-CENTER._1, CENTER._2, SIMULATION_BOUNDARY._1, SIMULATION_BOUNDARY._2)
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
    private var antSize = 0
    private var anthillFood = 0
    private var obstacleSize = 0
    private var enemiesSize = 0
    private var foodSize = 0

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


    reactions += {
      /**
       * When startButton is pressed the UiActor tell to Environment that the simulation
       * can be start with parameters.
       */
      case ButtonClicked(`startButton`) =>
        this.startButton.enabled = false
        this.stopButton.enabled = true
        this.restartButton.enabled = false
        this.reportButton.enabled = false

        if (!startFlag) {
          tellStart()
        } else {
          uiActor.tell(RestartSimulation(), uiActor)
        }

      /**
       * When stopButton is pressed the UiActor tell to self StopSimulation.
       * The timer will be stopped.
       */
      case ButtonClicked(`stopButton`) =>
        this.startFlag = true
        this.startButton.enabled = true
        this.stopButton.enabled = false
        this.restartButton.enabled = true
        this.reportButton.enabled = true
        uiActor.tell(StopSimulation, uiActor)

      /**
       * When restartButton is pressed environment and reportManager actor will be recreated.
       */
      case ButtonClicked(`restartButton`) =>
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

      /**
       * When reportButton is pressed new frame will be show with report.
       */
      case ButtonClicked(`reportButton`) =>
        reportManager.tell(ShowAndSaveReport(), uiActor)

      /**
       * When buttonSetTime is pressed uiActor tell to self to set rate of timer.
       */
      case ButtonClicked(`buttonSetTime`) =>
        uiActor ! setRate(timeInput.text.toInt)

    }

    /**
     * Call first message of environment to start simulation.
     * In both case start and restart.
     */
    private def tellStart(): Unit = {
      environment.tell(StartSimulation(antSize, enemiesSize, obstacles = Some(obstacleSize),
        food = Some(foodSize), anthillFood = Some(anthillFood)), uiActor)
    }


    override def setParameters(antSize: Int, anthillFood: Int, foodSize: Int,
                               obstacleSize: Int, enemiesSize: Int): Unit = {
      this.antSize = antSize
      this.anthillFood = anthillFood
      this.obstacleSize = obstacleSize
      this.enemiesSize = enemiesSize
      this.foodSize = foodSize
    }
  }

}
