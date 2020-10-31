package model.environment

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import common.geometry.Vector2D
import common.geometry.Vector2DFactory.ZeroVector2D
import common.message.EnvironmentMessage.{Ready, Repaint}
import common.message.SharedMessage._
import model.Drawable
import model.environment.data.EnvironmentInfo
import model.environment.elements.EnvironmentElements
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class EnvironmentTest extends TestKit(ActorSystem("environment-test"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val topLeftCorner: (Int, Int) = (0, 0)
  val width = 100
  val height = 100
  val boundary: Boundary = Boundary(topLeftCorner._1, topLeftCorner._2, width, height)

  val antsFilter: Drawable => Boolean = {
    case _: ForagingAntInfo => true
    case _: PatrollingAntInfo => true
    case _ => false
  }

  val insectsFilter: Drawable => Boolean = {
    case _: ForagingAntInfo => true
    case _: PatrollingAntInfo => true
    case _: EnemyInfo => true
    case _ => false
  }

  "Environment without obstacles" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-1")
    var initialPosition = ZeroVector2D()
    var newPosition = ZeroVector2D()

    "spawn an ant" should {
      val nAnts = 1
      environment ! StartSimulation(nAnts,0,None,None,0.0)
      sender expectMsg Ready
      environment ! Clock(1)
      val result1 = sender.expectMsgType[Repaint]

      "receive its initial position" in {
        initialPosition = (result1.info find antsFilter get).position
      }
    }
    "make ant move" should {
      environment ! Clock(2)
      val result2 = sender.expectMsgType[Repaint]

      "receive its new position" in {
        newPosition = (result2.info find antsFilter get).position
      }
      "receive no more messages" in {
        sender.expectNoMessage()
      }
      "check if ant moved" in {
        assert(initialPosition != newPosition)
      }
      "check if ant didn't go outside boundary" in {
        import model.environment.elements.EnvironmentElements.BoundaryHasInside
        assert(EnvironmentElements checkPositionIsInsideObstacle(boundary, newPosition))
      }
    }
  }

  "Environment without obstacles" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-2")
    val nAnts = 10

    "spawn multiple ants" should {
      environment ! StartSimulation(nAnts, 0, obstacles = None, food = None, 0.0)
      sender expectMsg Ready
      environment ! Clock(1)
      val result1 = sender.expectMsgType[Repaint]

      "receive all their positions" in {
        val positionsCount = result1.info count antsFilter
        assert(positionsCount >= nAnts)
      }
    }
    "make them move" should {
      environment ! Clock(2)
      var positions: Seq[Vector2D] = Seq.empty
      val result2 = sender.expectMsgType[Repaint]

      "receive all their new positions" in {
        positions = (result2.info filter antsFilter).map(_.position)
        assert(positions.size >= nAnts)
      }
      "receive no more messages" in {
        sender.expectNoMessage()
      }
      "check that no ant went outside boundary" in {
        import EnvironmentElements.BoundaryHasInside
        assert(positions.forall(x => elements.EnvironmentElements checkPositionIsInsideObstacle(boundary, x)))
      }
    }
  }

  "Environment with obstacles and food resources" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val nAnts = 100
    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-3")

    "spawn ants and make them move" should {
      environment ! StartSimulation(nAnts, 0, None, None, 0.0)
      sender expectMsg Ready
      environment ! Clock(1)
      val result = sender.expectMsgType[Repaint]

      "receive all their positions" in {
        val positionsCount = result.info count antsFilter
        assert(positionsCount >= nAnts)
      }
    }
  }

  "Environment" when {
    val sender = TestProbe()
    implicit val senderRef: ActorRef = sender.ref

    val nAnts = 50
    val nEnemies = 50
    val environment = system.actorOf(Environment(EnvironmentInfo(boundary)), name = "env-actor-5")

    "spawn ants and enemies" should {
      environment ! StartSimulation(nAnts, nEnemies, None, None, 0.0)
      sender expectMsg Ready
      environment ! Clock(1)
      val result = sender.expectMsgType[Repaint]

      "receive all their initial positions" in {
        val positionsCount = result.info count insectsFilter
        assert(positionsCount >= nAnts + nEnemies)
      }
    }
    "make them move" should {
      environment ! Clock(2)
      var positions: Seq[Vector2D] = Seq.empty

      "receive their new positions" in {
        /* We cannot assume nothing about number of positions received because of fights between insects */
        val result = sender.expectMsgType[Repaint]
        positions = (result.info filter antsFilter).map(_.position)
        sender.expectNoMessage()
      }
      "check that no insect went outside boundary" in {
        import EnvironmentElements.BoundaryHasInside
        assert(positions.forall(x => elements.EnvironmentElements checkPositionIsInsideObstacle(boundary, x)))
      }

    }
  }
}
