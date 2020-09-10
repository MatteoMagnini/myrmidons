package view.scene


import akka.actor.{ActorSystem, Props}
import model.{Boundary, Environment, EnvironmentInfo}
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.event.ActionEvent
import scalafx.scene.control.{Button, Label, Separator, ToggleButton, ToolBar}
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.text.Text
import scalafx.Includes._
import utility.Messages.{Clock, StartSimulation}
import view.actor.{UiActor, UiMessage}

/**
 * BorderPane with box for managing simulation
 * and a short legend to understand color entities.
 */

case class SimulationPane() extends BorderPane {
  /* Custom canvas */
  private val canvas = new MyrmidonsCanvas()
  private val system = ActorSystem("Myrmidons-system")
  private val uiActor = system.actorOf(Props(new UiActor(canvas, this)))
  val boundary = Boundary(0,0, 50, 50)
  val environment = system.actorOf(Environment(EnvironmentInfo(uiActor, boundary)), name = "env-actor")
  var step = new Text("1")

  /* ToolBar for manage ant simulation */
   var toolBox: ToolBar = new ToolBar {
    val population = new Text("0")
    private val startButton = new Button("Start") {
      handleEvent(ActionEvent.Action) {
        _: ActionEvent =>
          environment.tell(StartSimulation(1, Seq.empty, true),uiActor)
          environment.tell(Clock(step.text.value.toInt), uiActor)
      }
    }

    val stopButton = new Button("Stop")

    content = List(
      startButton,
      stopButton,
      new Separator,
      new Label("Step:"),
      step,
      new Label("population:"),
      population)
  }

  /* ToolBar for understand entities color */
  private val legendBox: ToolBar = new ToolBar {
    val ant = new Text("Ant")
    val food = new Text("Food")
    val obstacle = new Text("Obstacle")
    val anthill = new Text("Anthill")
    val insect = new Text("Insect")
    val pheromone = new Text("Pheromone")
    food.fill = Color.Red
    ant.fill = Color.Black
    obstacle.fill = Color.Brown
    insect.fill = Color.Grey
    anthill.fill = Color.Green
    pheromone.fill = Color.Violet

    content = List(
      ant, new Separator,
      food, new Separator,
      obstacle, new Separator,
      anthill, new Separator,
      insect, new Separator,
      pheromone)
  }

  top = toolBox
  center = new Pane {

    children = canvas
  }
  bottom = legendBox

  /* Initialize Canvas with predefined entities */
  //canvas.initializeCanvas()



}
