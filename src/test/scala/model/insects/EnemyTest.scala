package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.anthill.{Anthill, AnthillInfo}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry.{Vector2D, ZeroVector2D}
import utility.Messages.{Clock, FoodNear, FoodPheromones, Move, NewPosition, UpdateInsect}

class EnemyTest extends TestKit(ActorSystem("InsectTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val c = Constant
  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "an enemy" when {

    val startingInfo = EnemyInfo(senderRef)
    val enemy = system.actorOf(Enemy(startingInfo,senderRef), "enemy-0")

    "performing random walk" should {

      "start walking randomly" in {
        enemy ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        enemy ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(result2.info.energy == 100 + c.ENERGY_RW)
        sender expectNoMessage
      }

      "keep moving" in {
        enemy ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        enemy ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(result2.info.energy == 100 + c.ENERGY_RW * 2)
        sender expectNoMessage
      }

      //TODO: this will change
      "if find a food resource, eat it and register an energy increase" in {
        enemy ! FoodNear
        val result = sender.expectMsgType[UpdateInsect]
        assert(result.info.energy == 100)
      }
    }

  }
}

