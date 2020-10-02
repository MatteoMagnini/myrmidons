package model.insects

import akka.actor.{ActorRef, Props}
import model.insects.competences.RandomWalk
import model.insects.info.EnemyInfo
import utility.Messages.{Clock, Context, FoodNear, NewPosition, UpdateInsect}

class Enemy(override val info: EnemyInfo,
            override val environment: ActorRef) extends Insect[EnemyInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val competences = List(RandomWalk[EnemyInfo]())

  private def defaultBehaviour(data: EnemyInfo): Receive = {

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,competences)(context, environment, self, newData, defaultBehaviour)

    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d)
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    case FoodNear(_) => // println(s"Enemy ${info.id} near food")//TODO: MUST NOT RECEIVE THIS MESSAGE

    case Context(_) => sender ! Context(Some(context))

    case x => println("Enemies: Should never happen, received message: " + x + " from " + sender)
  }
}

object Enemy {
  def apply(info: EnemyInfo, environment: ActorRef): Props =
    Props(classOf[Enemy], info, environment)
}
