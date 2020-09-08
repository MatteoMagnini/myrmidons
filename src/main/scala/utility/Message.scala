package utility

import model.insects.{Entity, InsectInfo}

sealed trait Message

case class Clock(value: Int) extends Message
case class InsectUpdate(info: InsectInfo) extends Message
case class FoodPheromones(entities: Iterable[Entity])