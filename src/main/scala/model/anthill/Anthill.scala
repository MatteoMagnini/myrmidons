// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package model.anthill

import akka.actor.{Actor, ActorRef, Props}
import model.Drawable
import model.insects.competences._
import model.insects.info.{ForagingAntInfo, PatrollingAntInfo}
import model.insects.{ForagingAnt, PatrollingAnt}
import utility.Messages._
import utility.geometry.Vectors.doubleInRange
import utility.geometry._


case class AnthillInfo(override val position: Vector2D,
                       radius: Double,
                       foodAmount: Double,
                       maxFoodAmount: Double) extends Drawable {

  def incFood(delta: Double): AnthillInfo =
    this.copy(foodAmount = if (foodAmount + delta > maxFoodAmount) maxFoodAmount else foodAmount + delta)

  def decFood(delta: Double): AnthillInfo =
    this.copy(foodAmount = if (foodAmount - delta < 0) 0 else foodAmount - delta)
}

object AnthillInfo {
  def apply(position: Vector2D, radius: Double = 3, foodAmount: Double = 0, maxFoodAmount: Double = 10000): AnthillInfo =
    new AnthillInfo(position, radius, foodAmount, maxFoodAmount)
}

case class Anthill(info: AnthillInfo, environment: ActorRef) extends Actor {

  override def receive: Receive = defaultBehaviour(info)

  def defaultBehaviour(data: AnthillInfo): Receive = {

    case StoreFood(delta) =>
      context become defaultBehaviour(data.incFood(delta))

    case EatFood(delta) =>
      val newDelta = if (data.foodAmount > delta) delta else data.foodAmount
      val newData = data.decFood(newDelta)
      sender ! EatFood(newDelta)
      context become defaultBehaviour(newData)

    case AntTowardsAnthill(position, maxSpeed, inertia, noise, antIsIn) =>
      val dist = info.position - position
      if (!antIsIn && dist.|| <= data.radius) {
        sender ! UpdateAnthillCondition(true)
        environment.tell(Move(position, ZeroVector2D()), sender)
      } else {
        val rad = dist /\
        val delta = OrientedVector2DWithNoise(rad, maxSpeed, noise)
        val delta2 = OrientedVector2D((delta >> (inertia * INERTIA_FACTOR))./\, doubleInRange(MIN_VELOCITY, MAX_VELOCITY))
        environment.tell(Move(position, delta2), sender)
      }

    case Clock(value) =>
    /*  val antBirthValue = data.foodAmount / data.maxFoodAmount * Random.nextDouble()
      /* Random birth of ants */
      if (antBirthValue > 0.2) {
        environment ! AntBirth(value)
        self ! StoreFood(if (data.foodAmount < 10) - data.foodAmount else - 10)
      }*/
    environment ! UpdateAnthill(data)

    case CreateEntities(nAnts: Int, foragingPercentage: Double) =>

      /** Returns ants and enemies references, creating ants from the center of boundary */
      val nForaging = (nAnts * foragingPercentage).ceil.toInt
      val foragingAnts = (0 until nForaging).map(i => {
        i -> context.actorOf(ForagingAnt(ForagingAntInfo(self, id = i, position = info.position), sender), s"f-ant-$i")
      }).toMap
      val nPatrolling = nForaging + (nAnts * (1 - foragingPercentage)).toInt
      val patrollingAnts = (nForaging until nPatrolling).map(i => {
        i -> context.actorOf(PatrollingAnt(PatrollingAntInfo(self, id = i, position = info.position), sender), s"p-ant-$i")
      }).toMap
      sender ! NewEntities(foragingAnts ++ patrollingAnts)
  }
}

object Anthill {
  def apply(info: AnthillInfo, environment: ActorRef): Props =
    Props(classOf[Anthill], info, environment)
}

