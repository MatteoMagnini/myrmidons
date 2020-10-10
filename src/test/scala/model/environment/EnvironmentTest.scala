package model.environment

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.environment.elements.EnvironmentElements
import model.environment.info.EnvironmentInfo
import model.insects.info.{ForagingAntInfo, PatrollingAntInfo}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Messages._
import utility.geometry.{Vector2D, ZeroVector2D}

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
      environment ! StartSimulation(nAnts, 0, obstacles = None, food = None)
      sender expectMsg Ready
      environment ! Clock(1)

      "receive its initial position" in {
        val result = sender.expectMsgType[Repaint]
        println(result.info)
        initialPosition = result.info.filter {
          case _: ForagingAntInfo => true
          case _ => false
        }.head.position
      }
    }
    "make ant move" should {
      environment ! Clock(2)

      "receive its new position" in {
        val result = sender.expectMsgType[Repaint]
        newPosition = result.info.filter {
          case _: ForagingAntInfo => true
          case _ => false
        }.head.position
      }
      "receive no more messages" in {
        sender.expectNoMessage()
      }
      "check if ant moved" in {
        assert(initialPosition != newPosition)
      }
      "check if ant didn't go outside boundary" in {
        import model.environment.elements.EnvironmentElements.BoundaryHasInside
        assert(EnvironmentElements.checkHasInside(boundary, newPosition))
      }
    }
  }

  "Environment without obstacles" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-2")
    val nAnts = 10

    "spawn multiple ants" should {
      environment ! StartSimulation(nAnts, 0, obstacles = None, food = None)
      sender expectMsg Ready
      environment ! Clock(1)

      "receive all their positions" in {
        val result = sender.expectMsgType[Repaint]
        val positionsCount = result.info.count {
          case _: ForagingAntInfo => true
          case _: PatrollingAntInfo => true
          case _ => false
        }
        assert(positionsCount >= nAnts)
      }
    }
    "make them move" should {
      environment ! Clock(2)
      val positions: Seq[Vector2D] = Seq.empty

      "receive all their new positions" in {
        val result = sender.expectMsgType[Repaint]
        val positions = result.info.filter {
          case _: ForagingAntInfo => true
          case _: PatrollingAntInfo => true
          case _ => false
        }
        assert(positions.size >= nAnts)
      }
      "receive no more messages" in {
        sender.expectNoMessage()
      }
      "check that no ant went outside boundary" in {
        import EnvironmentElements.BoundaryHasInside
        assert(positions.forall(x => elements.EnvironmentElements.checkHasInside(boundary, x)))
      }
    }
  }

  "Environment with obstacles and food resources" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val nAnts = 10
    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-3")

    "spawn ants and make them move" should {
      environment ! StartSimulation(nAnts, 0)
      sender expectMsg Ready
      environment ! Clock(1)
    }
    "receive all their positions" in {
      val result = sender.expectMsgType[Repaint]
      val positionsCount = result.info.count {
        case _: ForagingAntInfo => true
        case _: PatrollingAntInfo => true
        case _ => false
      }
      assert(positionsCount >= nAnts)
    }
  }

 "Environment with an ant" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val nAnts = 2
    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-4")
    var nAntsPreBirth = 0
    var nAntsPostBirth = 0

    "other ants born" should {
      environment ! StartSimulation(nAnts, 0)
      sender expectMsg Ready
      environment ! Clock(1)
      val result = sender.expectMsgType[Repaint]
      nAntsPreBirth = result.info.count {
        case _: ForagingAntInfo => true
        case _ => false
      }
      environment ! AntBirth(1)
      environment ! Clock(2)
      val result2 = sender.expectMsgType[Repaint]
      nAntsPostBirth = result2.info.count {
        case _: ForagingAntInfo => true
        case _ => false
      }

      "see ants number increased" in {
        assert(nAntsPostBirth > nAntsPreBirth)
      }
    }
  }
}
