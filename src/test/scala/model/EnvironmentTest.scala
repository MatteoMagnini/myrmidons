package model

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Messages.{Clock, StartSimulation, UpdateInsect}

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
    val boundary = Boundary(0, 0, 10, 10)
    val environment = system.actorOf(Environment(EnvironmentInfo(senderRef, boundary)), name = "env-actor")
    "started" should {
      val nAnts = 1
      environment ! StartSimulation(nAnts, Seq.empty)
      environment ! Clock(1)
      "create ants" in {
        val result = sender.expectMsgType[UpdateInsect]
        println(result.info.position)
      }
      "send a clock message" in {

      }
    }
  }
}
