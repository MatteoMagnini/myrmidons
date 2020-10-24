package view.controller

import java.awt.event.{KeyEvent, KeyListener}
import view.scene.MyrmidonsPanel

trait Keyboard extends KeyListener {
  def myrmidonsPanel: MyrmidonsPanel
}

object Keyboard {

  def apply(panel: MyrmidonsPanel): Keyboard = new KeyboardImpl(panel)

  private[view] class KeyboardImpl(override val myrmidonsPanel: MyrmidonsPanel) extends Keyboard {

    private val PLUS = KeyEvent.VK_ADD
    private val MINUS = KeyEvent.VK_SUBTRACT
    private val UP = KeyEvent.VK_UP
    private val DOWN = KeyEvent.VK_DOWN
    private val RIGHT = KeyEvent.VK_RIGHT
    private val LEFT = KeyEvent.VK_LEFT

    /**
     * For each event panel will draw entities in different way.
     *
     * @param keyEvent a key event happened in panel.
     */
    override def keyPressed(keyEvent: KeyEvent): Unit = {
      keyEvent.getKeyCode match {
        case PLUS =>
          myrmidonsPanel.zoomIn()
        case MINUS =>
          myrmidonsPanel.zoomOut()
        case UP =>
          myrmidonsPanel.goNorth()
        case LEFT =>
          myrmidonsPanel.goEast()
        case DOWN =>
          myrmidonsPanel.goSouth()
        case RIGHT =>
          myrmidonsPanel.goWest()

        case _ =>
      }
    }

    override def keyReleased(keyEvent: KeyEvent): Unit = {}

    override def keyTyped(keyEvent: KeyEvent): Unit = {}
  }

}
