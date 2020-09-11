package model

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.environment.{Boundary, Environment, EnvironmentInfo}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry.ZeroVector2D
import utility.Messages.{Clock, RepaintInsects, StartSimulation}

class EnvironmentTest extends TestKit(ActorSystem("environment-test"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "Environment without obstacles" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val boundary = Boundary(0, 0, 100, 100)
    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-1")
    var initialPosition = ZeroVector2D()
    var newPosition = ZeroVector2D()

    "spawn an ant" should {
      val nAnts = 1
      environment ! StartSimulation(nAnts, Seq.empty, centerSpawn = true)
      environment ! Clock(1)

      "receive its initial position" in {
        val result = sender.expectMsgType[RepaintInsects]
        initialPosition = result.info.head.position
      }
    }
    "make ant move" should {
      environment ! Clock(2)

      "receive its new position" in {
        val result = sender.expectMsgType[RepaintInsects]
        newPosition = result.info.head.position
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
    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-2")
    val nAnts = 10

    "spawn multiple ants" should {
      environment ! StartSimulation(nAnts, Seq.empty, centerSpawn = true)
      environment ! Clock(1)

      "receive all their positions" in {
        val result = sender.expectMsgType[RepaintInsects]
        assert(result.info.size == nAnts)
      }
    }
    "make them move" should {
      environment ! Clock(2)
      "receive all their new positions" in {
        val result = sender.expectMsgType[RepaintInsects]
        assert(result.info.size == nAnts)
      }

      "receive no more messages" in {
        sender.expectNoMessage()
      }
    }
  }

  "Environment with an obstacle" when {
    import BorderedEntityFactory._
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val nAnts = 10
    val boundary = Boundary(0, 0, 100, 100)
    val obstacle: Bordered = createRandomSimpleObstacle(boundary.left, boundary.top, boundary.width, boundary.height)
    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-3")

    "spawn ants and make them move" should {
      environment ! StartSimulation(nAnts, Seq(obstacle), centerSpawn = true)
      environment ! Clock(1)
    }
    "receive all their positions" in {
      val result = sender.expectMsgType[RepaintInsects]
      assert(result.info.size == nAnts)
    }
  }
  //TODO find a pretty way to test borders and obstacles collisions

}
