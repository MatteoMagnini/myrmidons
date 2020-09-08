package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.{Clock, FoodPheromones, InsectUpdate}
import utility.Geometry.Vector2D

class InsectTest extends TestKit(ActorSystem("InsectTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "Foraging Ant" must {

    val ant = system.actorOf(ForagingAnt(id = 0,ForagingAntInfo(),senderRef), "ant-0")

    "perform random walk" in {
      ant ! Clock(1)
      val result = sender.expectMsgType[InsectUpdate]
      assert(result.info.position != Vector2D(0,0))
      val clock = sender.expectMsgType[Clock]
      assert(clock.value == 1)
      sender expectNoMessage
    }

    "multiple times" in {
      ant ! Clock(2)
      val result = sender.expectMsgType[InsectUpdate]
      assert(result.info.position != Vector2D(0,0))
      val clock = sender.expectMsgType[Clock]
      assert(clock.value == 2)
      sender expectNoMessage
    }

  }

  "Foraging Ant perceiving food pheromones" must {

    val ant = system.actorOf(ForagingAnt(id = 0,ForagingAntInfo(),senderRef), "ant-1")

    "perform food pheromone taxis" in {
      val pheromones = List(Entity(Vector2D(10,0),0.5))
      ant ! FoodPheromones(pheromones)
      ant ! Clock(1)
      val result = sender.expectMsgType[InsectUpdate]
      assert(result.info.position == Vector2D(5,0))
      val clock = sender.expectMsgType[Clock]
      assert(clock.value == 1)
      sender expectNoMessage
    }

    "multiple times" in {
      val pheromones = List(Entity(Vector2D(5,0),0.5))
      ant ! FoodPheromones(pheromones)
      ant ! Clock(2)
      val result = sender.expectMsgType[InsectUpdate]
      assert(result.info.position == Vector2D(7.5,0))
      val clock = sender.expectMsgType[Clock]
      assert(clock.value == 2)
      sender expectNoMessage
    }
  }

}
