package model.insects

import akka.actor.{ActorRef, Props}
import model.insects.Enemies._
import model.insects.competences.{Die, RandomWalk}
import model.insects.info.EnemyInfo
import common.RichActor._
import common.message.EnvironmentMessage.NewPosition
import common.message.InsectMessage.{KillInsect, UpdateInsect}
import common.message.SharedMessage.{Clock, Context}

/**
 * An enemy with state and behaviour.
 *
 * @param info        of the enemy
 * @param environment of the simulation
 */
class Enemy(override val info: EnemyInfo,
            override val environment: ActorRef) extends Insect[EnemyInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val competences = List(Die[EnemyInfo](defaultBehaviour), RandomWalk[EnemyInfo](defaultBehaviour))

  private def defaultBehaviour(data: EnemyInfo): Receive = {

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incrementTime()
      subsumption(newData, competences)(context, environment, self, newData)

    case NewPosition(position, delta) =>
      val newData = data.updatePosition(position).updateInertia(delta)
      if (data.position ~~ (position, 1E-7)) {
        environment ! KillInsect(data)
      } else {
        environment ! UpdateInsect(newData)
      }
      context >>> defaultBehaviour(newData)

    case KillInsect(_) =>
      context >>> defaultBehaviour(data.updateEnergy(-MAX_ENERGY))

    case Context(_) => sender ! Context(Some(context))

    case _ => //Do nothing
  }
}

object Enemy {
  def apply(info: EnemyInfo, environment: ActorRef): Props =
    Props(classOf[Enemy], info, environment)
}
