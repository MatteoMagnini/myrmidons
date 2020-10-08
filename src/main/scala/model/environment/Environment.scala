package model.environment

import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef, Props}
import model.Fights.Fight
import model.anthill.{Anthill, AnthillInfo}
import model.environment.elements.{Food, Obstacle}
import model.insects._
import model.insects.info._
import utility.Messages._
import utility.PheromoneSeq._
import model.insects.info.SpecificInsectInfo
import utility.geometry.{RandomVector2DInCircle, RandomVector2DInSquare, Vector2D}
import model.environment.elements.EnvironmentElements._
import utility.Parameters.Environment._
import model.environment.elements.EnvironmentElements.FoodHasInside
import Implicits._

/** Environment actor
 *
 * @param state environment internal state
 */
class Environment(state: EnvironmentInfo) extends Actor with ActorLogging {

  override def receive: Receive = defaultBehaviour(state)

  private def defaultBehaviour(state: EnvironmentInfo): Receive = {

    case StartSimulation(nAnts: Int, nEnemies: Int, obstaclesPresence, foodPresence) =>
      import Implicits._
      val anthillInfo = AnthillInfo(state.boundary.center, ANTHILL_RADIUS , FOOD_AMOUNT)
      val anthill = context.actorOf(Anthill(anthillInfo, self), name = "anthill")
      anthill ! CreateEntities(nAnts, 0.5)

      val enemies = (0 until nEnemies).map(i => {
        val randomPosition = RandomVector2DInSquare(state.boundary.topLeft.x, state.boundary.topRight.x)
        i -> context.actorOf(Enemy(EnemyInfo(id = i, position = randomPosition), self), s"enemy-$i")
      }).toMap
      // TODO: incapsulate inside obstacle
      val obstacles = if (obstaclesPresence.isDefined) Obstacle.createRandom(obstaclesPresence.get, anthillInfo.position, (50,150)).toSeq
      else Seq.empty

      val foods = if (foodPresence.isDefined) (0 until foodPresence).map(_ =>
        Food.createRandomFood(anthillInfo.position, FOOD_RADIUS._1, FOOD_RADIUS._2)) else Seq.empty

      context >>> defaultBehaviour(EnvironmentInfo(Some(sender), state.boundary,
         obstacles, foods, anthill, Some(anthillInfo)).addEnemies(enemies))

    case NewEntities(ants: Map[Int, ActorRef]) =>
      state.gui.get ! Ready
      context >>> defaultBehaviour(state.addAnts(ants))

    case Clock(value: Int) =>
      state.ants.values.foreach(_ ! Clock(value))
      state.ants.values.foreach(_ ! FoodPheromones(state.pheromones))
      state.enemies.values.foreach(_ ! Clock(value))
      state.anthill.get ! Clock(value)
      context >>> defaultBehaviour(state.updatePheromones(state.pheromones.tick()))

    case Move(position: Vector2D, delta: Vector2D) =>
      CollisionsInterceptor.checkCollisions(self, sender, state, position, delta)

    case TakeFood(delta, position) =>
      val food = state.foods.filter(f => f.position ~~ (position, 1E-7))
      if (food.nonEmpty) {
        sender ! TakeFood(delta, position)
        context >>> defaultBehaviour(state.updateFood(food.head, food.head - delta))
      } else {
        sender ! TakeFood(0, position)
      }

    case UpdateInsect(info: SpecificInsectInfo[x]) =>
      sendInfoToGUI(state.updateInsectInfo(info))

    case UpdateAnthill(anthillInfo: AnthillInfo) =>
      context >>> defaultBehaviour(state.updateAnthillInfo(Some(anthillInfo)))

    case AntBirth(clock: Int) =>
      /*Generate an id that doesn't exist for sure to avoid name conflicts*/
      // TODO: fai questo in anthill e probabilità di patrolling o foraging
      val antId = state.ants.size + clock
      val birthPosition = state.anthillInfo.position
      val ant = context.actorOf(ForagingAnt(ForagingAntInfo(state.anthill, id = antId, position = birthPosition, time = clock - 1), self), s"ant-$antId")
      ant ! Clock(clock)
      context >>> defaultBehaviour(state.addAnt(antId, ant))

    case KillInsect(info: InsectInfo) =>
      context.stop(sender)
      val newData = state.removeInsect(info)
      sendInfoToGUI(newData)

    case AddFoodPheromone(pheromone: FoodPheromone, threshold: Double) =>
      context >>> defaultBehaviour(state.addPheromone(pheromone, threshold))
  }

  private implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become behaviour
  }

  private def sendInfoToGUI(info: EnvironmentInfo): Unit = {
    /* When all insects return their positions, environment send them to GUI */

    if ((info.foragingAntsInfo.size + info.patrollingAntsInfo.size == info.ants.size)
      && (info.enemiesInfo.size == info.enemies.size)) {

      val fights = findFights(info.foragingAntsInfo, info.enemiesInfo)
      val updatedInfo = handleFights(info, fights)

      info.gui.get ! Repaint(info.anthillInfo.get +: (info.foragingAntsInfo ++ info.patrollingAntsInfo ++ info.enemiesInfo ++
        info.obstacles ++ info.foods ++ info.pheromones ++ fights).toSeq)
      context >>> defaultBehaviour(updatedInfo.emptyInsectInfo())
    } else context >>> defaultBehaviour(info)
  }

  private def findFights(antsInfo: Iterable[ForagingAntInfo], enemiesInfo: Iterable[EnemyInfo]): Iterable[Fight[ForagingAntInfo, EnemyInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)

  private def handleFights(info: EnvironmentInfo, fights: Iterable[Fight[ForagingAntInfo, EnemyInfo]]): EnvironmentInfo = {

    import model.Fights._
    import model.Fights.InsectFight._
    import Implicits._

    var updatedInfo = info
    for (loser <- losers(fights)) {
      loser match {
        case Left(ant) =>
          context.stop(info.ants(ant))
          updatedInfo = updatedInfo.removeAnt(ant)
        case Right(enemy) =>
          context.stop(info.enemies(enemy))
          updatedInfo = updatedInfo.removeEnemy(enemy)
      }
    }
    updatedInfo
  }
}

object Environment {
  def apply(state: EnvironmentInfo): Props = Props(classOf[Environment], state)
}

object Implicits {
  implicit def extractOption[X](value: Option[X]): X = value.get

  implicit def entity2Id[X <: SpecificInsectInfo[X]](entity: X): Int = entity.id
}