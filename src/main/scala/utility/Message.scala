package utility

sealed trait Message

  object Messages {

    case class Clock(value: Int) extends Message

    case class MoveMessage(pos: (Double, Double), dir: (Int, Int)) extends Message

  }