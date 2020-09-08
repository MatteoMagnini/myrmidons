package model.insects

import akka.actor.{Actor, ActorRef, Props}
import utility.Messages.{Clock, FoodPheromones}

trait Insect extends Actor {
  def id: Int
  def info: InsectInfo
}

case class ForagingAnt(override val id: Int,
                       override val info: ForagingAntInfo,
                       environment: ActorRef) extends Insect {

  def subsumption(competences: Competence*): Competence = competences.filter(c => c.hasPriority(info)).take(1).last

  override def receive: Receive = {

    case Clock(t) if t == info.time + 1 =>
      info.incTime(); subsumption(FoodPheromoneTaxis,RandomWalk)(context, environment, info)

    case FoodPheromones(entities) => entities.foreach(e => info.pheromoneSensor.addEntity(e))

    case _ => println("Should never happen")
  }
}

object ForagingAnt {
  def apply(id: Int, info: InsectInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], id, info, environment)
}