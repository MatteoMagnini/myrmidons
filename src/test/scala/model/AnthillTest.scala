package model

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, wordspec}
import org.scalatest.wordspec.AnyWordSpecLike
import model.Anthill
import utility.Geometry.Vector2D
import utility.Messages.{StorageFood, TakeFood}

class AnthillTest extends TestKit(ActorSystem("environment-test"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "An anthill" must {

    val anthill = system.actorOf(Anthill(AnthillInfo(Vector2D(2 , 3),0,100)))

    "storage some food" in {

      val quantity = 20;

      anthill ! StorageFood(quantity)
      sender.expectNoMessage()
      anthill ! TakeFood(quantity)
      val received = sender.expectMsgType[TakeFood]
      assert(received.quantity == quantity)
    }

    "return a all stored food if there is a request with more food than actual anthill stored food" in {
      val quantity = 20;
      anthill ! StorageFood(quantity)
      sender.expectNoMessage()
      val t = 500
      anthill ! TakeFood(500)
      val received = sender.expectMsgType[TakeFood]
      assert(received.quantity == quantity)
    }

    "return zero if storage is empty" in {
      anthill ! TakeFood(10)
      sender.expectMsg(TakeFood(0))
    }
  }

}