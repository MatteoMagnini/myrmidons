package model

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry.ZeroVector2D
import utility.Messages.{Clock, StartSimulation, UpdateInsect}

class EnvironmentTest extends TestKit(ActorSystem("environment-test"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

/*
  "Environment without obstacles" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val boundary = Boundary(0, 0, 100, 100)
    val environment = system.actorOf(Environment(EnvironmentInfo(senderRef, boundary)), name = "env-actor-1")
    var initialPosition = ZeroVector2D()
    var newPosition = ZeroVector2D()

    "create an ant" should {
      val nAnts = 1
      environment ! StartSimulation(nAnts, Seq.empty, centerSpawn = true)

      "receive its initial position" in {
        val result = sender.expectMsgType[UpdateInsect]
        initialPosition = result.info.position
      }
    }
    "make ant move" should {
      environment ! Clock(1)

      "receive its new position" in {
        val result = sender.expectMsgType[UpdateInsect]
        newPosition = result.info.position
      }
      "receive no more messages" in {
        sender.expectNoMessage()
      }
      "check if ant moved" in {
        assert(initialPosition != newPosition)
      }
    }
  }

  "Environment without obstacles" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val boundary = Boundary(0, 0, 100, 100)
    val environment = system.actorOf(Environment(EnvironmentInfo(senderRef, boundary)), name = "env-actor-2")
    val nAnts = 10

    "create multiple ants" should {
      environment ! StartSimulation(nAnts, Seq.empty, centerSpawn = true)

      "receive all their positions" in {
        (0 until nAnts).map(_ => sender.expectMsgType[UpdateInsect])
      }
    }
    "make them move" should {
      environment ! Clock(1)
      "receive all their new positions" in {
        (0 until nAnts).map(_ => sender.expectMsgType[UpdateInsect])
      }
      "receive no more messages" in {
        sender.expectNoMessage()
      }
    }
  }

  "Environment with an obstacle" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val boundary = Boundary(0, 0, 100, 100)
    val environment = system.actorOf(Environment(EnvironmentInfo(senderRef, boundary)), name = "env-actor-3")

    "create ants" should {
      val nAnts = 1
      environment ! StartSimulation(nAnts, Seq.empty)
      environment ! Clock(1)
    }
  }

 */
}
