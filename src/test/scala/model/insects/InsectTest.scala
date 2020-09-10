package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry.ZeroVector2D
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

  "Foraging Ant" must {

    val ant = system.actorOf(ForagingAnt(ForagingAntInfo(),senderRef), "ant-0")

    "perform random walk" in {
      ant ! Clock(1)
      val result1 = sender.expectMsgType[Move]
      ant ! NewPosition(result1.start >> result1.delta, result1.delta)
      val result2 = sender.expectMsgType[UpdateInsect]
      assert(result2.info.position != ZeroVector2D())
      assert(result2.info.energy == 99)
      val clock = sender.expectMsgType[Clock]
      assert(clock.value == 1)
      sender expectNoMessage
    }

    "multiple times" in {
      ant ! Clock(2)
      val result1 = sender.expectMsgType[Move]
      ant ! NewPosition(result1.start >> result1.delta, result1.delta)
      val result2 = sender.expectMsgType[UpdateInsect]
      assert(result2.info.position != ZeroVector2D())
      assert(result2.info.energy == 98)
      val clock = sender.expectMsgType[Clock]
      assert(clock.value == 2)
      sender expectNoMessage
    }

  }

  "Foraging Ant perceiving food pheromones" must {

    val ant = system.actorOf(ForagingAnt(ForagingAntInfo(id = 1),senderRef), "ant-1")

    /*"perform food pheromone taxis" in {
      val pheromones = List(Entity(Vector2D(10,0),0.5))
      ant ! FoodPheromones(pheromones)
      ant ! Clock(1)
      val result1 = sender.expectMsgType[Move]
      ant ! NewPosition(result1.start + result1.delta)
      val result2 = sender.expectMsgType[InsectUpdate]
      assert(result2.info.position == Vector2D(5,0))
      assert(result2.info.energy == 98.5)
      val clock = sender.expectMsgType[Clock]
      assert(clock.value == 1)
      sender expectNoMessage
    }

    "multiple times" in {
      val pheromones = List(Entity((5,0),0.5))
      ant ! FoodPheromones(pheromones)
      ant ! Clock(2)
      val result1 = sender.expectMsgType[Move]
      ant ! NewPosition(result1.start + result1.delta)
      val result2 = sender.expectMsgType[InsectUpdate]
      assert(result2.info.position == Vector2D(7.5,0))
      assert(result2.info.energy == 97)
      val clock = sender.expectMsgType[Clock]
      assert(clock.value == 2)
      sender expectNoMessage
    }*/
  }

}
