package model.insects

import akka.actor.{ActorRef, Props}
import model.environment.pheromones.DangerPheromone
import model.insects.competences.{Die, RandomWalk}
import model.insects.info.EnemyInfo
import utility.Messages._
import utility.Parameters.Insects.Ants.ForagingAnt.MAX_ENERGY
import utility.Parameters.Pheromones.DangerPheromoneInfo.DANGER_PHEROMONE_MERGING_THRESHOLD

class Enemy(override val info: EnemyInfo,
            override val environment: ActorRef) extends Insect[EnemyInfo] {

  override def receive: Receive = defaultBehaviour(info)

  private val competences = List(Die[EnemyInfo](), RandomWalk[EnemyInfo]())

  private def defaultBehaviour(data: EnemyInfo): Receive = {

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,competences)(context, environment, self, newData, defaultBehaviour)

    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d).updateEnergy(info.energy)
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    case FoodNear(_) => // println(s"Enemy ${info.id} near food")//TODO: MUST NOT RECEIVE THIS MESSAGE

    /**
     * Just for tests
     */
    case Context(_) => sender ! Context(Some(context))

    case KillInsect(_) =>
      context >>> defaultBehaviour(data.updateEnergy(-MAX_ENERGY))

    case x => println("Enemies: Should never happen, received message: " + x + " from " + sender +info.time)
  }
}

object Enemy {
  def apply(info: EnemyInfo, environment: ActorRef): Props =
    Props(classOf[Enemy], info, environment)
}
