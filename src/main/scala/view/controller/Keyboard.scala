package view.controller

import java.awt.event.{KeyEvent, KeyListener}

import view.scene.MyrmidonsPanel.MyrmidonsPanelImpl

class Keyboard(panel: MyrmidonsPanelImpl) extends KeyListener{

  private val PLUS = KeyEvent.VK_ADD
  private val MINUS = KeyEvent.VK_SUBTRACT
  private val UP = KeyEvent.VK_UP
  private val DOWN = KeyEvent.VK_DOWN
  private val RIGHT = KeyEvent.VK_RIGHT
  private val LEFT = KeyEvent.VK_LEFT


  override def keyPressed(keyEvent: KeyEvent): Unit = {
    keyEvent.getKeyCode match {
      case PLUS =>
        panel.zoomIn()
      case MINUS =>
        panel.zoomOut()
      case UP =>
        panel.goNorth()
      case LEFT =>
        panel.goEast()
      case DOWN =>
        panel.goSouth()
      case RIGHT =>
        panel.goWest()

      case _ =>

    }

  }

  override def keyReleased(keyEvent: KeyEvent): Unit = {

  }

  override def keyTyped(keyEvent: KeyEvent): Unit = {

  }
}
