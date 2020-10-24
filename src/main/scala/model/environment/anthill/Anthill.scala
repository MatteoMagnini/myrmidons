package model.environment.anthill

import akka.actor.{Actor, ActorRef, Props}
import common.geometry.Vector2DFactory.{OrientedVector2D, OrientedVector2DWithNoise, ZeroVector2D}
import common.geometry.Vectors.doubleInRange
import common.message.AnthillMessage.{NewAnts, UpdateAnthill, UpdateAnthillCondition}
import common.message.EnvironmentMessage.CreateAnts
import common.message.InsectMessage.{AntTowardsAnthill, EatFood, Move, StoreFood}
import common.message.SharedMessage.Clock
import model.insects.competences._
import model.insects.info.{ForagingAntInfo, PatrollingAntInfo}
import model.insects.{ForagingAnt, PatrollingAnt}

class Anthill(info: AnthillInfo, environment: ActorRef) extends Actor {

  override def receive: Receive = defaultBehaviour(info)

  def defaultBehaviour(data: AnthillInfo): Receive = {

    case StoreFood(delta) =>
      context become defaultBehaviour(data.incrementFood(delta))

    case EatFood(delta) =>
      val newDelta = if (data.foodAmount > delta) delta else data.foodAmount
      val newData = data.decrementFood(newDelta)
      sender ! EatFood(newDelta)
      context become defaultBehaviour(newData)

    case AntTowardsAnthill(position, maxSpeed, inertia, noise, antIsIn) =>
      val dist = info.position - position
      if (!antIsIn && dist.|| <= data.radius) {
        sender ! UpdateAnthillCondition
        environment.tell(Move(position, ZeroVector2D()), sender)
      } else {
        val unboundedDelta = OrientedVector2DWithNoise(dist /\, maxSpeed, noise)
        val delta = OrientedVector2D((unboundedDelta >> (inertia * INERTIA_FACTOR))./\,
          doubleInRange(MIN_VELOCITY, MAX_VELOCITY))
        environment.tell(Move(position, delta), sender)
      }

    case Clock(_) =>
      environment ! UpdateAnthill(data)

    case CreateAnts(nAnts: Int, foragingPercentage: Double) =>
      val nForaging = (nAnts * foragingPercentage).ceil.toInt
      val foragingAnts = (0 until nForaging).map(i => {
        i -> context.actorOf(ForagingAnt(ForagingAntInfo(
          self, id = i, position = info.position), sender), s"f-ant-$i")
      }).toMap
      val nPatrolling = nForaging + (nAnts * (1 - foragingPercentage)).toInt
      val patrollingAnts = (nForaging until nPatrolling).map(i => {
        i -> context.actorOf(PatrollingAnt(PatrollingAntInfo(self, id = i, position = info.position),
          sender), s"p-ant-$i")
      }).toMap
      sender ! NewAnts(foragingAnts ++ patrollingAnts)
  }
}

object Anthill {
  def apply(info: AnthillInfo, environment: ActorRef): Props =
    Props(classOf[Anthill], info, environment)
}

