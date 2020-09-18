package view.scene

import scala.swing.event.ButtonClicked
import scala.swing.{Button, GridPanel}
import utility.Messages.AddRandomAnt

case class InteractionPanel(controlPane: ControlPane) extends GridPanel(10, 1) {

  private val addAnt = new Button("Add ant")
  private val killAnt = new Button("Kill ant")

  contents ++= Seq(addAnt, killAnt)
  listenTo(addAnt, killAnt)
  reactions += {
    case ButtonClicked(component) if component == addAnt =>
    //TODO next sprint
    //controlPane.environment.tell(AddRandomAnt(1, controlPane.stepText.text), controlPane.uiActor)
    case ButtonClicked(component) if component == killAnt =>
      print("This function will be implemented!")
    //TODO next sprint
    // controlPane.environment.tell(KillAnt, controlPane.uiActor)
  }
}
