package view.controller

import view.scene.MyrmidonsPanel
import java.awt.event.{MouseEvent, MouseListener}

class Mouse(panel: MyrmidonsPanel) extends MouseListener {

  override def mouseClicked(mouseEvent: MouseEvent): Unit =
    panel.requestFocusInWindow

  override def mousePressed(mouseEvent: MouseEvent): Unit =
    panel.requestFocusInWindow

  override def mouseReleased(mouseEvent: MouseEvent): Unit =
    panel.requestFocusInWindow

  override def mouseEntered(mouseEvent: MouseEvent): Unit =
    panel.requestFocusInWindow

  override def mouseExited(mouseEvent: MouseEvent): Unit = {

  }
}
