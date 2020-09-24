package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.anthill.{Anthill, AnthillInfo}
import model.environment.FoodPheromone
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry._
import utility.Messages._

class InsectTest extends TestKit(ActorSystem("InsectTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val c = Constant
  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "Foraging Ant" when {

    val startingInfo = ForagingAntInfo(senderRef)
    val ant = system.actorOf(ForagingAnt(startingInfo,senderRef), "ant-0")
    val DELTA = 1E-10
    val startingIntensity = 100.0

    "performing random walk" should {

      "start walking randomly" in {
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(result2.info.energy == 100 + c.ENERGY_RW)
        sender expectNoMessage
      }

      "keep moving" in {
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(result2.info.energy == 100 + c.ENERGY_RW * 2)
        sender expectNoMessage
      }
    }

    "an ant has low energy" should {

      val anthillInfo = AnthillInfo(ZeroVector2D(),foodAmount = 10)
      val anthill = system.actorOf(Anthill(anthillInfo,senderRef), "anthill")
      val startingPosition = Vector2D(3.2,0)
      val startingEnergy = 38
      val info = ForagingAntInfo(anthill, id = 1, position = startingPosition, energy = startingEnergy )
      val ant = system.actorOf(ForagingAnt(info,senderRef), "ant-1")

      "go back to the anthill" in {
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val finalPosition = Vector2D(3.2 - c.MAX_VELOCITY,0)
        val finalEnergy = startingEnergy + c.ENERGY_RW
        val result2 = sender.expectMsgType[UpdateInsect]
        //assert(result2.info.position == finalPosition)
        assert(result2.info.inertia == result1.delta)
        assert(result2.info.energy == finalEnergy)
        sender expectNoMessage
      }

      "spend one turn to enter inside the anthill" in {
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val finalPosition = Vector2D(3.2 - c.MAX_VELOCITY,0)
        val finalEnergy = startingEnergy + c.ENERGY_RW + c.ENERGY_RW
        val result2 = sender.expectMsgType[UpdateInsect]
        //assert(result2.info.position == finalPosition)
        assert(result2.info.inertia == result1.delta)
        assert(result2.info.energy == finalEnergy)
        sender expectNoMessage
      }

      "eat the reserve inside the anthill" in {
        ant ! Clock(3)
        val finalPosition = Vector2D(3.2 - c.MAX_VELOCITY,0)
        val finalEnergy = startingEnergy + c.ENERGY_RW + c.ENERGY_RW + c.ENERGY_EATING + 5
        val result1 = sender.expectMsgType[UpdateInsect]
        //assert(result1.info.position == finalPosition)
        assert(result1.info.inertia == ZeroVector2D())
        assert(result1.info.energy == finalEnergy)
        sender expectNoMessage
      }
    }


    "performing food pheromone taxis" should {
      val info = ForagingAntInfo(senderRef)
      val pheromones = Seq(FoodPheromone(ZeroVector2D(), DELTA, startingIntensity))

      "update the sensor in presence of pheromones" in {
        assert(info.foodPheromones.isEmpty)
        val info2 = info.updateFoodPheromones(pheromones).asInstanceOf[ForagingAntInfo]
        assert(info2.foodPheromones.nonEmpty)
      }

      val ant = system.actorOf(ForagingAnt(ForagingAntInfo(senderRef, id = 2),senderRef), "ant-2")

      "perform food pheromone taxis" in {
        val pheromones = Seq(FoodPheromone(Vector2D(5,0), DELTA,startingIntensity ))
        ant ! FoodPheromones(pheromones)
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        //assert(result2.info.position == Vector2D(5,0))
        assert(result2.info.energy == 100 + c.ENERGY_FPT)
        sender expectNoMessage
      }

      "multiple times" in {
        val pheromones = Seq(FoodPheromone(Vector2D(2,0),DELTA, startingIntensity))
        ant ! FoodPheromones(pheromones)
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        //assert(result2.info.position == Vector2D(7.5,0))
        assert(result2.info.energy == 100 + 2 * c.ENERGY_FPT)
        sender expectNoMessage
      }
    }
  }

  "Foraging ant" when {
    val ant = system.actorOf(ForagingAnt(ForagingAntInfo(senderRef, energy = 0),senderRef), "ant-3")
    "has no more energy" should {
      "die" in {
        ant ! Clock(1)
        sender.expectMsgType[KillAnt]
        sender expectNoMessage
      }
    }
  }
}
