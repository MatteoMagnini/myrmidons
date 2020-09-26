package model.insects

import akka.actor.{ActorRef, Props}
import model.insects.ForagingAntConstant.{MAX_ENERGY, MAX_FOOD, STARTING_ENERGY, STARTING_FOOD_AMOUNT, STARTING_POSITION, STARTING_TIME}
import utility.Geometry.{Vector2D, ZeroVector2D}
import utility.Messages.{Clock, FoodNear, NewPosition, UpdateInsect}

class Enemy(override val info: EnemyInfo,
            override val environment: ActorRef) extends Insect {

  /**
   * Use of the subsumption architecture to model the final emerging behaviour.
   *
   * @param competences a set of competences that the ant is able to perform.
   * @return the competence with highest priority.
   */
  private def subsumption(data: InsectInfo, competences: Competence*): Competence = competences.filter(c => c.hasPriority(data)).take(1).last

  override def receive: Receive = defaultBehaviour(info)

  private def defaultBehaviour(data: InsectInfo): Receive = {

    case Clock(t) if t == data.time + 1 =>
      val newData = data.incTime()
      subsumption(newData,
        //EatFromTheAnthill, // if inside anthill it's behaviour became like parasite
        RandomWalk)(context, environment, self, newData, defaultBehaviour)

    case NewPosition(p, d) =>
      val newData = data.updatePosition(p).updateInertia(d)
      environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    case FoodNear(position) =>
      val newData = data.updateFoodPosition(Some(position))
      //environment ! UpdateInsect(newData)
      context become defaultBehaviour(newData)

    case x => println("Enemies: Should never happen, received message: " + x + " from " + sender)
  }
}

object Enemy {
  def apply(info: InsectInfo, environment: ActorRef): Props =
    Props(classOf[Enemy], info, environment)
}

case class EnemyInfo(override val anthill: ActorRef,
                     override val isInsideTheAnthill: Boolean,
                     override val foodPosition: Option[Vector2D],
                     override val id: Int,
                     override val position: Vector2D,
                     override val inertia: Vector2D,
                     override val energy: Double,
                     override val time: Int,
                     foodAmount: Double) extends InsectInfo {

  override def updatePosition(newPosition: Vector2D): InsectInfo =
    this.copy(position = newPosition)

  override def updateInertia(newInertia: Vector2D): InsectInfo =
    this.copy(inertia = newInertia)

  override def updateEnergy(amount: Double): InsectInfo =
    this.copy(energy = if (energy + amount > MAX_ENERGY) MAX_ENERGY else energy + amount)

  override def incTime(): InsectInfo =
    this.copy(time = time + 1)

  override def updateAnthillCondition(value: Boolean): InsectInfo =
    this.copy(isInsideTheAnthill = value)

  override def updateFoodPosition(position: Option[Vector2D]): InsectInfo =
    this.copy(foodPosition = position)

  def incFood(amount: Double): EnemyInfo =
    this.copy(foodAmount = if (foodAmount + amount > MAX_FOOD) MAX_FOOD else foodAmount + amount)

  def freeFood(): EnemyInfo =
    this.copy(foodAmount = STARTING_FOOD_AMOUNT)
}

object EnemyInfo {
  def apply(anthill: ActorRef, id: Int = 0, position: Vector2D = STARTING_POSITION, energy: Double = STARTING_ENERGY, time: Int = STARTING_TIME): EnemyInfo =
    new EnemyInfo(anthill, false, None, id, position, ZeroVector2D(), energy, time, STARTING_FOOD_AMOUNT)
}
