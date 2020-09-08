package model.insects

import akka.actor.{Actor, ActorRef, Props}
import utility.Clock

trait Insect extends Actor {
  def id: Int
  def info: InsectInfo
}

case class ForagingAnt(override val id: Int,
                       override val info: InsectInfo,
                       environment: ActorRef) extends Insect {

  //TODO: for the moment just take the last one.
  def subsumption(competences: Competence*): Competence = competences.last

  override def receive: Receive = {

    case Clock(t) if t > info.time => info.incTime(); subsumption(RandomWalk)(context, environment, info)

    case _ => println("Should never happen")
  }
}

object ForagingAnt {
  def apply(id: Int, info: InsectInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], id, info, environment)
}