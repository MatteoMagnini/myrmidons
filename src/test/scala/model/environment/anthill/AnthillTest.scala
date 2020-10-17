package model.environment.anthill

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import common.Messages._
import common.geometry.Vector2D

class AnthillTest extends TestKit(ActorSystem("AnthillTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  /* Ignore AntBirthMessages */
  sender.ignoreMsg {
    case AntBirth(_) => true
    case _ => false
  }

  "The anthill" when {

    val startingPosition = Vector2D(500,500)
    val startingInfo = AnthillInfo(startingPosition)
    val anthill = system.actorOf(Anthill(startingInfo, senderRef), name = "anthill")

    "initialized" should {

      "be correctly initialized" in {
        anthill ! Clock(0)
        sender.expectMsg(UpdateAnthill(startingInfo))
        sender.expectNoMessage()
      }
    }

    "interacting with insects" should {

      val radius = startingInfo.radius
      val foodToStore = 7.25
      val foodToTake = 2.5
      val newFoodToTake = 10.1
      val newFoodToStore = startingInfo.maxFoodAmount * 2

      "increment the food storage if an insect drop some food" in {

        val newInfo = AnthillInfo(startingPosition, radius, foodToStore)

        anthill ! StoreFood(foodToStore)
        anthill ! Clock(1)
        sender.expectMsg(UpdateAnthill(newInfo))
        sender.expectNoMessage()
      }

      "decrease the food storage if an insect take some food" in {

        val newInfo = AnthillInfo(startingPosition, radius, foodToStore - foodToTake)

        anthill ! EatFood(foodToTake)
        sender.expectMsg(EatFood(foodToTake))
        sender.expectNoMessage()
        anthill ! Clock(2)
        sender.expectMsg(UpdateAnthill(newInfo))
        sender.expectNoMessage()

      }

      "decrease to 0 if an insect request exceeds the current food amount" in {

        val newInfo = AnthillInfo(startingPosition)

        anthill ! EatFood(newFoodToTake)
        sender.expectMsg(EatFood(foodToStore - foodToTake))
        sender.expectNoMessage()
        anthill ! Clock(3)
        sender.expectMsg(UpdateAnthill(newInfo))
        sender.expectNoMessage()

      }

      "increase to max amount if an insect store more food than allowed by the anthill" in {

        val newInfo = AnthillInfo(startingPosition, radius, startingInfo.maxFoodAmount)

        anthill ! StoreFood(newFoodToStore)
        anthill ! Clock(4)

        sender.expectMsg(UpdateAnthill(newInfo))
        sender.expectNoMessage()

      }

    }
  }

}