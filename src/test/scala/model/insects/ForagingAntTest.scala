package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import common.geometry.Vectors._
import common.geometry._
import common.message.AnthillMessage.{UpdateAnthill, UpdateAnthillCondition}
import common.message.EnvironmentMessage.{FoodNear, NewPosition, Pheromones}
import common.message.InsectMessage.{AddPheromone, KillInsect, Move, TakeFood, UpdateInsect}
import common.message.Message
import common.message.SharedMessage.Clock
import common.rTree.RTree.Tree
import common.rTree.RTreeProlog
import model.environment.anthill.{Anthill, AnthillInfo}
import model.environment.elements.{Food, Obstacle, ObstacleFactory}
import model.environment.pheromones.FoodPheromone
import model.insects.Ants.ForagingAnt._
import model.insects.competences._
import model.insects.info.ForagingAntInfo
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ForagingAntTest extends TestKit(ActorSystem("ForagingAntTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "Foraging Ant" when {

    val startingInfo = ForagingAntInfo(senderRef)
    val ant = system.actorOf(ForagingAnt(startingInfo, senderRef), "ant-0")
    val DELTA = 1E-10

    "performing random walk" should {

      "start walking randomly" in {
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(result2.info.energy == STARTING_ENERGY + ENERGY_RANDOM_WALK)
        sender expectNoMessage
      }

      "keep moving" in {
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(~=(result2.info.energy, STARTING_ENERGY + ENERGY_RANDOM_WALK * 2))
        sender expectNoMessage
      }
    }

    "an ant has low energy" should {

      val anthillInfo = AnthillInfo(ZeroVector2D(), foodAmount = 10)
      val anthill = system.actorOf(Anthill(anthillInfo, senderRef), "anthill")
      val startingPosition = Vector2D(3.2, 0)
      val startingEnergy = 38
      val info = ForagingAntInfo(anthill, id = 1, position = startingPosition, energy = startingEnergy)
      val ant = system.actorOf(ForagingAnt(info, senderRef), "ant-1")

      "go back to the anthill" in {
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val finalEnergy = startingEnergy + ENERGY_RANDOM_WALK
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(anthillInfo.position --> result2.info.position < anthillInfo.radius)
        assert(result2.info.inertia == result1.delta)
        assert(~=(result2.info.energy, finalEnergy))
        sender expectNoMessage
      }

      "spend one turn to enter inside the anthill" in {
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val finalEnergy = startingEnergy + ENERGY_RANDOM_WALK + ENERGY_RANDOM_WALK
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(anthillInfo.position --> result2.info.position < anthillInfo.radius)
        assert(result2.info.inertia == result1.delta)
        assert(result2.info.energy == finalEnergy)
        sender expectNoMessage
      }

      "eat the reserve inside the anthill" in {
        ant ! Clock(3)
        val finalEnergy = startingEnergy + ENERGY_RANDOM_WALK + ENERGY_RANDOM_WALK +
          ENERGY_EATING + FOOD_ENERGY_CONVERSION * FOOD_EATEN_PER_STEP
        val result1 = sender.expectMsgType[UpdateInsect]
        assert(anthillInfo.position --> result1.info.position < anthillInfo.radius)
        assert(result1.info.inertia == ZeroVector2D())
        assert(result1.info.energy == finalEnergy)
        sender expectNoMessage
      }
    }

    "performing food pheromone taxis" should {
      val info = ForagingAntInfo(senderRef)
      val startingPheromoneIntensity = 10.0
      val pheromones = Seq(FoodPheromone(ZeroVector2D(), x => x - DELTA, startingPheromoneIntensity))

      "update the sensor in presence of pheromones" in {
        assert(info.foodPheromones.isEmpty)
        val info2 = info.updateFoodPheromones(pheromones)
        assert(info2.foodPheromones.nonEmpty)
      }

      import common.rTree.getPheromoneAsNode
      val ant = system.actorOf(ForagingAnt(ForagingAntInfo(senderRef, id = 2), senderRef), "ant-2")
      val tree: Tree[Int] = Tree()
      val engine = RTreeProlog()

      "perform food pheromone taxis" in {
        val pheromones = Map(1 -> FoodPheromone(Vector2D(5, 0), x => x - DELTA, startingPheromoneIntensity))
        ant ! Pheromones(pheromones, engine.insertNode((pheromones.head._1, pheromones.head._2), tree))
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position --> pheromones.last._2.position < result1.start --> pheromones.last._2.position)
        assert(result2.info.energy == STARTING_ENERGY + ENERGY_FOOD_PHEROMONE_TAXIS)
        sender expectNoMessage
      }

      "multiple times" in {
        val pheromones = Map(1 -> FoodPheromone(Vector2D(8, 1), x => x - DELTA, startingPheromoneIntensity))
        ant ! Pheromones(pheromones, engine.insertNode((pheromones.head._1, pheromones.head._2), tree))
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position --> pheromones.last._2.position < result1.start --> pheromones.last._2.position)
        assert(~=(result2.info.energy, STARTING_ENERGY + 2 * ENERGY_FOOD_PHEROMONE_TAXIS))
        sender expectNoMessage
      }
    }
  }

  "Foraging ant" when {

    val ant = system.actorOf(ForagingAnt(ForagingAntInfo(senderRef, energy = 0), senderRef), "ant-3")

    "has no more energy" should {

      "die" in {
        ant ! Clock(1)
        sender.expectMsgType[KillInsect]
        sender expectNoMessage
      }
    }
  }

  "Foraging ant" when {

    val food = Food((2.0, 2.0), MAX_FOOD * 2, ObstacleFactory((2, 2), Food.radius(MAX_FOOD.toInt * 2), 16))
    val anthillInfo = AnthillInfo(ZeroVector2D())
    val anthill = system.actorOf(Anthill(anthillInfo, senderRef), "anthill2")
    val startingAntPosition = (1, 3)
    val startingEnergy = 80
    val ant = system.actorOf(ForagingAnt(
      ForagingAntInfo(anthill, position = startingAntPosition, energy = startingEnergy), senderRef), "ant-4")

    "touching food" should {

      "be aware of it" in {
        ant ! Clock(1)
        sender.expectMsgType[Move]
        ant ! FoodNear(food.position)
        ant ! NewPosition(startingAntPosition >> Vector2D(0.1, 0), ZeroVector2D())
        val result1 = sender.expectMsgType[UpdateInsect]
        assert(result1.info.asInstanceOf[ForagingAntInfo].foodIsNear)
        sender expectNoMessage
      }

      "pick some food" in {
        ant ! Clock(2)
        val result1 = sender.expectMsgType[TakeFood]
        assert(result1.delta == MAX_FOOD)
        assert(result1.position == food.position)
        ant ! TakeFood(MAX_FOOD, food.position)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.asInstanceOf[ForagingAntInfo].foodAmount == MAX_FOOD)
        sender expectNoMessage
      }

      "return to anthill or drop pheromone" in {
        ant ! Clock(3)
        val result1 = sender.expectMsgType[Message]
        result1 match {
          case m: Move =>
            ant ! NewPosition(m.start >> m.delta, m.delta)
            val result2 = sender.expectMsgType[UpdateInsect]
            assert(anthillInfo.position --> result2.info.position < anthillInfo.radius)

          case d: AddPheromone =>
            assert(d.pheromone.position equals implicitly[Vector2D](startingAntPosition) >> Vector2D(0.1, 0))
            sender.expectMsgType[UpdateInsect]
        }
        sender expectNoMessage
      }
    }

    "near anthill while carrying food" should {

      "store food into the anthill" in {
        ant.tell(UpdateAnthillCondition(true), anthill)
        ant ! Clock(4)
        val result1 = sender.expectMsgType[UpdateInsect]
        assert(result1.info.asInstanceOf[ForagingAntInfo].foodAmount == 0)
        sender expectNoMessage
      }

      "anthill has received the food" in {
        anthill ! Clock(5)
        val result1 = sender.expectMsgType[UpdateAnthill]
        assert(result1.info.foodAmount == MAX_FOOD)
        sender expectNoMessage
      }
    }
  }
}