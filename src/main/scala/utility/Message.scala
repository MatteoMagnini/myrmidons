package utility
import model.Vector2D

sealed trait Message

  object Messages {

    case class StartSimulation(nAnts: Int) extends Message

    case class Clock(value: Int) extends Message

    case class MoveMessage(pos: Vector2D, dir: (Int, Int)) extends Message

  }