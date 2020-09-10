package utility
import model.Obstacle
import model.insects.{Entity, InsectInfo}
import utility.Geometry._

sealed trait Message

  object Messages {

    case class StartSimulation(nAnts: Int, obstacles: Seq[Obstacle]) extends Message

    case class Clock(value: Int) extends Message

    case class Move(start: Vector, delta: Vector) extends Message

    case class FoodPheromones(entities: Iterable[Entity]) extends Message

    case class UpdateInsect(info: InsectInfo) extends Message

    case class NewPosition(position: Vector, delta: Vector) extends Message

  }
