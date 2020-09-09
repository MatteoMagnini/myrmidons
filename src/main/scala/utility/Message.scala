package utility

import model.insects.{Entity, InsectInfo}
import utility.Geometry._

sealed trait Message

case class Clock(value: Int) extends Message
case class InsectUpdate(info: InsectInfo) extends Message
case class FoodPheromones(entities: Iterable[Entity]) extends Message
case class Move(start: Vector, delta: Vector) extends Message
case class NewPosition(position: Vector) extends Message