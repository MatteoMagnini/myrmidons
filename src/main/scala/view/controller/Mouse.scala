package view.controller

import java.awt.event.{MouseEvent, MouseListener, MouseWheelEvent, MouseWheelListener}

import view.scene.MyrmidonsPanel


class Mouse(panel: MyrmidonsPanel) extends MouseListener {

  override def mouseClicked(mouseEvent: MouseEvent): Unit =
    panel.requestFocusInWindow

  override def mousePressed(mouseEvent: MouseEvent): Unit =
    panel.requestFocusInWindow

  override def mouseReleased(mouseEvent: MouseEvent): Unit =
    panel.requestFocusInWindow

  override def mouseEntered(mouseEvent: MouseEvent): Unit =
    panel.requestFocusInWindow

  override def mouseExited(mouseEvent: MouseEvent): Unit = {}
}

class MouseWheel(panel: MyrmidonsPanel) extends MouseWheelListener {

  override def mouseWheelMoved(mouseWheelEvent: MouseWheelEvent): Unit = {
    if (mouseWheelEvent.getWheelRotation < 0) {
      panel.zoomIn()
    }
    else {
      panel.zoomOut()
    }
  }
}
