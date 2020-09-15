package model

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.environment.{Boundary, Environment, EnvironmentInfo}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry.ZeroVector2D
import utility.Messages.{Clock, Repaint, StartSimulation}

class EnvironmentTest extends TestKit(ActorSystem("environment-test"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }
  val topLeftCorner:(Int, Int) = (0,0)
  val width = 100
  val height = 100
  val boundary = Boundary(topLeftCorner._1, topLeftCorner._2, width, height)

  "Environment without obstacles" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-1")
    var initialPosition = ZeroVector2D()
    var newPosition = ZeroVector2D()

    "spawn an ant" should {
      val nAnts = 1
      environment ! StartSimulation(nAnts, centerSpawn = true)
      environment ! Clock(1)

      "receive its initial position" in {
        val result = sender.expectMsgType[Repaint]
        initialPosition = result.info.head.position
      }
    }
    "make ant move" should {
      environment ! Clock(2)

      "receive its new position" in {
        val result = sender.expectMsgType[Repaint]
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

    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-2")
    val nAnts = 10

    "spawn multiple ants" should {
      environment ! StartSimulation(nAnts, centerSpawn = true)
      environment ! Clock(1)

      "receive all their positions" in {
        val result = sender.expectMsgType[Repaint]
        assert(result.info.size == nAnts)
      }
    }
    "make them move" should {
      environment ! Clock(2)
      "receive all their new positions" in {
        val result = sender.expectMsgType[Repaint]
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
    val obstacle: Bordered = createRandomSimpleObstacle(topLeftCorner._1, topLeftCorner._2, width, height)
    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-3")

    "spawn ants and make them move" should {
      environment ! StartSimulation(nAnts, centerSpawn = true)
      environment ! Clock(1)
    }
    "receive all their positions" in {
      val result = sender.expectMsgType[Repaint]
      assert(result.info.size == nAnts)
    }
  }
  //TODO find a pretty way to test borders and obstacles collisions

}
