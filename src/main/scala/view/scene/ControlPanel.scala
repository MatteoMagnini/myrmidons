package view.scene

import java.awt.event.ItemEvent

import akka.actor.{ActorRef, ActorSystem}
import common.message.SharedMessage.StartSimulation
import model.environment.data.EnvironmentInfo
import model.environment.{Boundary, Environment}
import view.actor.uiMessage.{RestartSimulation, ShowAndSaveReport, StopSimulation, setRate}
import view.actor.{ReportManager, ReportManagerInfo, UiActor, UiActorInfo}

import scala.swing.event.ButtonClicked
import scala.swing.{Button, CheckBox, FlowPanel, GridPanel, Label, Separator, TextField}

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

  /** Use to set each step in simulation  */
  def setAntPopulationLabel(newPopulation: String): Unit

  /** Use to set each ant size in simulation */
  def setStepLabel(newStep: String): Unit

  /** Use to set each anthill food in simulation */
  def setAnthillFoodAmount(newAnthillFoodAmount: String): Unit

}

object ControlPanel {
  def apply(myrmidonsPanel: MyrmidonsPanel): ControlPanel = new ControlPanelImpl(myrmidonsPanel)

  /** GridPane with simulation control button.
   *
   * @param myrmidonsPanel panel to pass at actor to draw entities.
   */
  private[view] class ControlPanelImpl(override val myrmidonsPanel: MyrmidonsPanel)
    extends GridPanel(2, 2) with ControlPanel {

    private val system = ActorSystem("Myrmidons-system")
    private val boundary = Boundary(-CENTER.width, CENTER.height, SIMULATION_BOUNDARY.width, SIMULATION_BOUNDARY.height)
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
    private val step = new Label("0")
    private val antPopulation = new Label("0")
    private val anthillFoodAmount = new Label("0")
    private var antSize = 0
    private var anthillFood = 0
    private var obstacleSize = 0
    private var enemiesSize = 0
    private var foodSize = 0

    var uiActor: ActorRef = _
    var environment: ActorRef = _
    var reportManager: ActorRef = _
    uiActor = system.actorOf(UiActor(UiActorInfo(myrmidonsPanel, this)))
    environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor")
    reportManager = system.actorOf(ReportManager(ReportManagerInfo()))
    var startFlag = false
    val pheromoneLayer = new CheckBox("Hide pheromones")

    this.stopButton.enabled = false
    this.restartButton.enabled = false
    this.reportButton.enabled = false


    override def setStepLabel(newStep: String): Unit =
      step.text = newStep

    override def setAntPopulationLabel(newPopulation: String): Unit =
      antPopulation.text = newPopulation

    override def setAnthillFoodAmount(newAnthillFoodAmount: String): Unit =
      anthillFoodAmount.text = newAnthillFoodAmount


    contents ++= Seq(new FlowPanel {
      contents ++= Seq(startButton, stopButton, restartButton,
        reportButton, timeLabel, timeInput, buttonSetTime, pheromoneLayer)
    },
      new FlowPanel {
        contents ++= Seq(stepLabel, step, populationLabel, antPopulation,
          new Separator(), anthillFoodAmountLabel, anthillFoodAmount)
      })


    pheromoneLayer.peer.addItemListener((_: ItemEvent) => {
      myrmidonsPanel.hidePheromones(pheromoneLayer.peer.isSelected)
    })
    listenTo(startButton, stopButton, restartButton, reportButton, buttonSetTime)


    reactions += {
      /*
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
          uiActor.tell(RestartSimulation, uiActor)
        }

      /*
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

      /*
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

        val uiRestart: ActorRef = system.actorOf(UiActor(UiActorInfo(myrmidonsPanel, this)))
        val reportRestart: ActorRef = system.actorOf(ReportManager(ReportManagerInfo()))
        uiActor = uiRestart
        reportManager = reportRestart
        val environmentRestart: ActorRef = system.actorOf(Environment(EnvironmentInfo(boundary)),
          name = s"env+${step.text}")
        environment = environmentRestart
        tellStart()

      /*
       * When reportButton is pressed new frame will be show with report.
       */
      case ButtonClicked(`reportButton`) =>
        reportManager.tell(ShowAndSaveReport, uiActor)

      /*
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
        food = Some(foodSize), anthillFood = anthillFood), uiActor)
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
