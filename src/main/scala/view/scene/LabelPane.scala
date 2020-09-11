package view.scene


import javax.swing.{JButton, JPanel}


/**
 * BorderPane with box for managing simulation
 * and a short legend to understand color entities.
 */

case class SimulationPane(canvas :MyrmidonsPanel) extends JPanel {

/*
  private val system = ActorSystem("Myrmidons-system")
  private val uiActor = system.actorOf(Props(new UiActor()))
  private val boundary = Boundary(0,0, 80 , 80)
  val environment: ActorRef = system.actorOf(Environment(EnvironmentInfo(uiActor, boundary)), name = "env-actor")
  //var step = new Text("1")
  //val nAnt = new Text("0")

  /* ToolBar for manage ant simulation */
   var toolBox: ToolBar = new ToolBar {

    private val startButton = new JButton("Start")
     startButton.addActionListener(_ => onClick())
     def onClick(): Unit = {
       val seqObstacle = Seq(new SimpleObstacle(Vector2D(30,30),6,6),
         new SimpleObstacle(Vector2D(60,60),6,6))
       environment.tell(StartSimulation(1000,seqObstacle , centerSpawn = true),uiActor)
       environment.tell(Clock(0), uiActor)
     }


    /*{
      handleEvent(ActionEvent.Action) {
        _: ActionEvent =>
        //  canvas.drawObstacle(20,20)
          val seqObstacle = Seq(new SimpleObstacle(Vector2D(30,30),6,6),
            new SimpleObstacle(Vector2D(60,60),6,6))
          environment.tell(StartSimulation(1000,seqObstacle , centerSpawn = true),uiActor)
          environment.tell(Clock(step.text.value.toInt), uiActor)
      }
    }*/


    val stopButton = new JButton("Stop")

    content = List(
      startButton,
      stopButton,
      new Separator,
      new Label("Step:"),
      step,
      new Label("N. Ants:"),
      nAnt)
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
 */
}
