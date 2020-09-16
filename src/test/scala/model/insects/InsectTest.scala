package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
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

  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "Foraging Ant" when {

    val startingInfo = ForagingAntInfo(senderRef)
    val ant = system.actorOf(ForagingAnt(startingInfo,senderRef), "ant-0")

    "performing random walk" should {

      "start walking randomly" in {
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(result2.info.energy == 99.7)
        sender expectNoMessage
      }

      "keep moving" in {
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(result2.info.energy == 99.4)
        sender expectNoMessage
      }

      "if find a food resource, eat it and register an energy increase" in {
        ant ! FoodNear
        val result = sender.expectMsgType[UpdateInsect]
        assert(result.info.energy == 100)
      }
    }

    "performing food pheromone taxis" should {
      val info = ForagingAntInfo(senderRef)
      val pheromones = List(Entity(ZeroVector2D(), 1))

      "update the sensor in presence of pheromones" in {
        assert(info.pheromoneSensor.entities.isEmpty)
        val info2 = info.addPheromones(pheromones)
        assert(info2.pheromoneSensor.entities.nonEmpty)
      }

      val ant = system.actorOf(ForagingAnt(ForagingAntInfo(senderRef, id = 1),senderRef), "ant-1")

      "perform food pheromone taxis" in {
        val pheromones = List(Entity(Vector2D(10,0),0.5))
        ant ! FoodPheromones(pheromones)
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position == Vector2D(5,0))
        assert(result2.info.energy == 98.5)
        sender expectNoMessage
      }

      "multiple times" in {
        val pheromones = List(Entity(Vector2D(5,0),0.5))
        ant ! FoodPheromones(pheromones)
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position == Vector2D(7.5,0))
        assert(result2.info.energy == 97)
        sender expectNoMessage
      }
    }
  }
}
