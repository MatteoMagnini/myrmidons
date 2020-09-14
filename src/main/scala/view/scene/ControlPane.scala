package view.scene

import akka.actor.{ActorRef, ActorSystem, Props}
import model.{Food, SimpleObstacle}
import model.environment.{Boundary, Environment, EnvironmentInfo}
import utility.Geometry.Vector2D
import utility.Messages.{Clock, StartSimulation}
import view.actor.UiActor

import scala.swing.event.MouseClicked
import scala.swing.{Button, FlowPanel, Label}

/**
 * FlowPanel which contains simulation button.
 *
 * @param myrmidonsPanel
 */
case class ControlPane(myrmidonsPanel: MyrmidonsPanel) extends FlowPanel {
import utility.Geometry.TupleOp._

  private val system = ActorSystem("Myrmidons-system")
  private val boundary = Boundary(0, 0, 80, 80)
  val seqObstacle = Seq(new SimpleObstacle(Vector2D(30, 30), 6, 6),
    new SimpleObstacle(Vector2D(60, 60), 6, 6), Food((20, 20), 20))
  private val uiActor = system.actorOf(Props(new UiActor(myrmidonsPanel, this)))
  val environment: ActorRef = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor")
  private val startButton = new Button("Start")
  private val stopButton = new Button("Stop")
  var stepText = new Label("0")
  var antPopulationText = new Label("0")
  private val stepLabel = new Label("Step:")
  private val populationLabel = new Label("Ants number:")

  contents ++= Seq(startButton, stopButton, stepLabel, stepText, populationLabel, antPopulationText)

  listenTo(startButton.mouse.clicks)
  /**
   * When startButton is pressed the UiActor tell to Environment that the simulation
   * can be start with a sequence of obstacles and numbers of ants.
   */
  reactions += {
    case _: MouseClicked => {
      environment.tell(StartSimulation(10, seqObstacle, centerSpawn = true), uiActor)
      environment.tell(Clock(1), uiActor)
    }
  }
  listenTo(stopButton.mouse.clicks)
  reactions += {
    case _: MouseClicked => {
      // TODO
    }
  }
}
