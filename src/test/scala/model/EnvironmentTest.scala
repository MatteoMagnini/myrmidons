package model

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Messages.StartSimulation

class EnvironmentTest extends TestKit(ActorSystem("environment-test"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "Environment" when {
    val environment = system.actorOf(Props[Environment], name = "env-actor")
    "started" should {
      val nAnts = 10
      environment ! StartSimulation(nAnts)
      "create ants" in {

      }
      "send a clock message" in {

      }
    }
  }

}
