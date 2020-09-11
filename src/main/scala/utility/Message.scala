package utility
import model.{Bordered, Obstacle}
import model.insects.{Entity, InsectInfo}
import utility.Geometry._

sealed trait Message

  object Messages {

    /** Message sent from GUI to environment, to start simulation.
      *
      * @param nAnts number of ants to be created
      * @param obstacles obstacles to put in environment
      * @param centerSpawn whether spawn ants from center of boundaries
      */
    case class StartSimulation(nAnts: Int, obstacles: Seq[Bordered], centerSpawn: Boolean = false) extends Message

    /** Message sent from GUI to environment and from environment to ants, to do a step in simulation.
      *
      * @param value clock value
      */
    case class Clock(value: Int) extends Message

    /** Message sent from ant to environment, to change position.
      *
      * @param start initial position
      * @param delta shift vector
      */
    case class Move(start: Vector, delta: Vector) extends Message

    case class FoodPheromones(entities: Iterable[Entity]) extends Message

    /** Message sent from ant to environment, to update its information.
      *
      * @param info ant information
      */
    case class UpdateInsect(info: InsectInfo) extends Message

    /** Message from environment to GUI, to repaint ants.
      *
      * @param info ants information
      */
    case class RepaintInsects(info: Seq[InsectInfo]) extends Message

    /** Message sent from environment to ant, to share its new position.
      *
      * @param position ant new position
      * @param inertia ant new inertia value
      */
    case class NewPosition(position: Vector, inertia: Vector) extends Message

    case class StorageFood(quantity: Int) extends Message

    case class TakeFood(quantity: Int) extends Message

  }
