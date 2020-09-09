package utility
import model.Vector2D
import model.insects.{Entity, InsectInfo}

sealed trait Message

  object Messages {

    case class StartSimulation(nAnts: Int) extends Message

    case class Clock(value: Int) extends Message

    case class MoveMessage(pos: Vector2D, delta: Vector2D) extends Message

    case class InsectUpdate(info: InsectInfo) extends Message

    case class FoodPheromones(entities: Iterable[Entity]) extends Message

    case class UpdateInsect(info: InsectInfo)

  }

