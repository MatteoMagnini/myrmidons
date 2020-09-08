package utility

import model.insects.InsectInfo

sealed trait Message

case class Clock(value: Int) extends Message
case class InsectUpdate(info: InsectInfo) extends Message