package view.scene

import akka.actor.{ActorRef, ActorSystem, Props}
import model.environment.{Boundary, Environment, EnvironmentInfo}
import utility.Messages.{Clock, StartSimulation}
import view.actor.UiActor
import view.actor.uiMessage.{RestartSimulation, StopSimulation}

import scala.swing.event.ButtonClicked
import scala.swing.{Button, FlowPanel, Label}

/**
 * FlowPanel which contains simulation button.
 *
 * @param myrmidonsPanel
 */
case class ControlPane(myrmidonsPanel: MyrmidonsPanel) extends FlowPanel {

  private val system = ActorSystem("Myrmidons-system")
  private val boundary = Boundary(0, 0, 800, 800)
  var uiActor : ActorRef = _
  var environment : ActorRef = _
  uiActor = system.actorOf(Props(new UiActor(myrmidonsPanel, this)))
  environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor")
  var startFlag = false

  var stepText = new Label("0")
  var antPopulationText = new Label("0")
  private val stepLabel = new Label("Step:")
  private val populationLabel = new Label("Ants number:")
  private val startButton = new Button("Start")
  private val stopButton = new Button("Stop")
  private val restartButton = new Button("Restart")
  this.stopButton.enabled = false
  this.restartButton.enabled = false
  contents ++= Seq(startButton, stopButton, restartButton, stepLabel, stepText, populationLabel, antPopulationText)

  listenTo(startButton, stopButton, restartButton)
  /**
   * When startButton is pressed the UiActor tell to Environment that the simulation
   * can be start with a sequence of obstacles and numbers of ants.
   */

  reactions += {
    case ButtonClicked(component) if component == startButton =>

      this.startButton.enabled = false
      this.stopButton.enabled = true
      this.restartButton.enabled = false
      if(!startFlag) {
        tellStart()
      }else{
        uiActor.tell(RestartSimulation(),uiActor)
      }
    case ButtonClicked(component) if component == stopButton =>
      this.startFlag = true
      this.startButton.enabled = true
      this.stopButton.enabled = false
      this.restartButton.enabled = true
      uiActor.tell(StopSimulation(false), uiActor)

    case ButtonClicked(component) if component == restartButton =>
      system.stop(environment)
      system.stop(uiActor)
      this.restartButton.enabled = false
      this.stopButton.enabled = true
      this.startButton.enabled = false
      val uiRestart: ActorRef = system.actorOf(Props(new UiActor(myrmidonsPanel, this)))
      uiActor = uiRestart
      val environmentRestart: ActorRef = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env")
      environment = environmentRestart
      tellStart()

  }
  private def tellStart(): Unit ={
    environment.tell(StartSimulation(1000, centerSpawn = true), uiActor)
    environment.tell(Clock(1), uiActor)
  }

}
