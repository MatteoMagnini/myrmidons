package model.insects

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import utility.Messages._

trait Insect extends Actor with ActorLogging {

  def info: InsectInfo
  def environment: ActorRef
}

case class ForagingAnt(override val info: ForagingAntInfo,
                       override val environment: ActorRef) extends Insect {

  def subsumption(competences: Competence*): Competence = competences.filter(c => c.hasPriority(info)).take(1).last

  override def receive: Receive = defaultBehaviour(info)

  private def defaultBehaviour(data: InsectInfo): Receive = {

    case Clock(t) if t == 0 => sender ! UpdateInsect(info)

    case Clock(t) if t == data.time + 1 =>
      subsumption(FoodPheromoneTaxis,RandomWalk)(context, environment, self, data.incTime(), defaultBehaviour)

    case NewPosition(p) =>
      val newData = data.updatePosition(p)
      environment ! UpdateInsect(newData)
      environment ! Clock(newData.time)
      context become defaultBehaviour(newData)

    case FoodPheromones(entities) =>
      context become defaultBehaviour(data.asInstanceOf[ForagingAntInfo].addPheromones(entities))

    case x => println("Should never happen, received message: " + x.getClass + " from " + sender)

  }
}

object ForagingAnt {
  def apply(info: InsectInfo, environment: ActorRef): Props =
    Props(classOf[ForagingAnt], info, environment)
}