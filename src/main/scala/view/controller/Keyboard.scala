package view.controller

import java.awt.event.{KeyEvent, KeyListener}

import view.scene.MyrmidonsPanel.MyrmidonsPanelImpl

class Keyboard(panel: MyrmidonsPanelImpl) extends KeyListener{

  private val PLUS = KeyEvent.VK_ADD
  private val MINUS = KeyEvent.VK_SUBTRACT
  private val W = KeyEvent.VK_W
  private val A = KeyEvent.VK_A
  private val S = KeyEvent.VK_S
  private val D = KeyEvent.VK_D
  private val UP = KeyEvent.VK_UP
  private val DOWN = KeyEvent.VK_DOWN
  private val RIGHT = KeyEvent.VK_RIGHT
  private val LEFT = KeyEvent.VK_LEFT

  override def keyTyped(keyEvent: KeyEvent): Unit = {

  }

  override def keyPressed(keyEvent: KeyEvent): Unit = {
    keyEvent.getKeyCode match {
      case PLUS =>
        panel.zoomIn()

      case MINUS =>
        panel.zoomOut()

      case UP =>
        up
      case W =>
        up

      case LEFT =>
        left
      case A =>
        left

      case DOWN =>
        down
      case S =>
        down

      case RIGHT =>
        right
      case D =>
        right

      case _ =>

    }

  }

  override def keyReleased(keyEvent: KeyEvent): Unit = {

  }

  private def up: Unit = {
    panel.goNorth()
  }

  private def left: Unit = {
    panel.goEast()
  }

  private def down: Unit = {
    panel.goSouth()
  }

  private def right: Unit = {
    panel.goWest()
  }
}
