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
        panel.drawEntities()

      case MINUS =>
        panel.zoomOut()
        panel.drawEntities()

      case UP =>
      case W =>
        panel.goNorth()
        panel.drawEntities()

      case LEFT =>
      case A =>
        panel.goEast()
        panel.drawEntities()

      case DOWN =>
      case S =>
        panel.goSouth()
        panel.drawEntities()

      case RIGHT =>
      case D =>
        panel.goWest()
        panel.drawEntities()

      case _ =>

    }
  }

  override def keyReleased(keyEvent: KeyEvent): Unit = {

  }
}
