package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.insects.info.EnemyInfo
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Messages.{Clock, Move, NewPosition, UpdateInsect}
import utility.geometry.ZeroVector2D

class EnemyTest extends TestKit(ActorSystem("InsectTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "an enemy" when {

    val startingInfo = EnemyInfo()
    val enemy = system.actorOf(Enemy(startingInfo,senderRef), "enemy-0")

    "performing random walk" should {

      "start walking randomly" in {
        enemy ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        enemy ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        sender expectNoMessage
      }

      "keep moving" in {
        enemy ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        enemy ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        sender expectNoMessage
      }
    }

  }
}

