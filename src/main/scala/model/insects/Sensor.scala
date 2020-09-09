package model.insects

import utility.Geometry._

/**
 * Abstraction for a real entity such obstacle, pheromones, etc.
 * @param position the relative position wrt the insect
 * @param intensity a value from 0 to 1 that indicates how strong the entity is perceived
 */
class Entity(val position: Vector, val intensity: Double)

object Entity {
  def apply( position: Vector, intensity: Double ): Entity = new Entity(position, intensity)
}

/**
 * A sensor is an abstraction for real insect biological sense like antennae.
 * With a sensor an insect can perceive entities (obstacles, pheromones, food, etc).
 */
trait Sensor {

  def entities: List[Entity]

  def addEntity(entity: Entity): Sensor

  def clearEntity(): Sensor

  def strongest: Option[Entity] =
    if (entities.isEmpty) None else Some(entities.toStream.sortWith((e1,e2) => e1.intensity > e2.intensity).last)

  def weightedSum: Vector =
    if (entities.isEmpty) ZeroVector2D() else entities.toStream.map(e => e.position * e.intensity).reduce(_+_)
}

case class ProximitySensor(override val entities: List[Entity]) extends Sensor {

  override def addEntity( entity: Entity ): Sensor = ProximitySensor(entity :: entities)

  override def clearEntity( ): Sensor = ProximitySensor()
}

object ProximitySensor {
  def apply(): Sensor = new ProximitySensor(List.empty)
}

case class PheromoneSensor(override val entities: List[Entity]) extends Sensor {

  override def addEntity( entity: Entity ): Sensor = PheromoneSensor(entity :: entities)

  override def clearEntity( ): Sensor = PheromoneSensor()
}

object PheromoneSensor {
  def apply(): Sensor = new PheromoneSensor(List.empty)
}
