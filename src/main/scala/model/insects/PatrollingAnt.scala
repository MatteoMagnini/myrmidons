package model.insects

import akka.actor.{ActorRef, Props}
import model.insects.competences.{DangerPheromoneTaxis, Die, EatFromTheAnthill, GoBackToHome, GoOutside, RandomWalk}
import model.insects.info.PatrollingAntInfo
import utility.Messages.{Clock, NewPosition, UpdateInsect}

case class PatrollingAnt (override val info: PatrollingAntInfo,
                     override val environment: ActorRef) extends Insect[PatrollingAntInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val competences = List(Die[PatrollingAntInfo](),
    GoOutside[PatrollingAntInfo](),
    EatFromTheAnthill[PatrollingAntInfo](),
    GoBackToHome[PatrollingAntInfo](),
    DangerPheromoneTaxis(),
    RandomWalk[PatrollingAntInfo]())

  private def defaultBehaviour(data: PatrollingAntInfo): Receive = {

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,competences)(context, environment, self, newData, defaultBehaviour)

    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d)
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    case x => //Discarding useless messages
  }
}

object PatrollingAnt {
  def apply(info: PatrollingAntInfo, environment: ActorRef): Props =
    Props(classOf[PatrollingAnt], info, environment)
}

