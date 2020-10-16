package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.anthill.{Anthill, AnthillInfo}
import model.environment.pheromones.DangerPheromone
import model.environment.pheromones.DangerPheromoneInfo._
import model.insects.competences._
import model.insects.info.PatrollingAntInfo
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Messages._
import utility.geometry.{Vector2D, ZeroVector2D}
import utility.rTree.RTree.Tree
import utility.rTree.RTreeProlog

class PatrollingAntTest extends TestKit(ActorSystem("PatrollingAntTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "A patrolling ant" when {

    import model.insects.Ants.PatrollingAnt._
    import utility.geometry.Vectors._

    "performing random walk" should {

      val startingInfo = PatrollingAntInfo(senderRef)
      val ant = system.actorOf(PatrollingAnt(startingInfo,senderRef), "ant-0")

      "start walking randomly" in {
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(~=(result2.info.energy, STARTING_ENERGY + ENERGY_RANDOM_WALK))
        sender expectNoMessage
      }

      "keep moving" in {
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position != ZeroVector2D())
        assert(~=(result2.info.energy,  STARTING_ENERGY + ENERGY_RANDOM_WALK * 2))
        sender expectNoMessage
      }

    }

    "perceiving danger pheromones" should {

      val startingPheromoneIntensity = 10.0
      val startingInfo = PatrollingAntInfo(senderRef)
      val ant = system.actorOf(PatrollingAnt(startingInfo,senderRef), "ant-1")
      import utility.rTree.getPheromoneAsNode
      val tree = Tree()
      val engine = RTreeProlog()

      "perform danger pheromones taxis" in {
        val pheromones = Map(1 -> DangerPheromone(Vector2D(5,0), x => x - DELTA,startingPheromoneIntensity ))
        ant ! Pheromones(pheromones, engine.insertNode((pheromones.head._1,pheromones.head._2),tree))
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(result2.info.position --> pheromones.last._2.position < result1.start --> pheromones.last._2.position)
        assert(~=(result2.info.energy, STARTING_ENERGY + ENERGY_DANGER_PHEROMONE_TAXIS))
        sender expectNoMessage
      }
    }

    "an ant has low energy" should {

      val anthillInfo = AnthillInfo(ZeroVector2D(),foodAmount = 10)
      val anthill = system.actorOf(Anthill(anthillInfo,senderRef), "anthill")
      val startingPosition = Vector2D(3.2,0)
      val startingEnergy = 38
      val info = PatrollingAntInfo(anthill, id = 1, position = startingPosition, energy = startingEnergy )
      val ant = system.actorOf(PatrollingAnt(info,senderRef), "ant-2")

      "go back to the anthill" in {
        ant ! Clock(1)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val finalEnergy = startingEnergy + ENERGY_RANDOM_WALK
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(anthillInfo.position --> result2.info.position < anthillInfo.radius)
        assert(result2.info.inertia == result1.delta)
        assert(result2.info.energy == finalEnergy)
        sender expectNoMessage
      }

      "spend one turn to enter inside the anthill" in {
        ant ! Clock(2)
        val result1 = sender.expectMsgType[Move]
        ant ! NewPosition(result1.start >> result1.delta, result1.delta)
        val finalEnergy = startingEnergy + ENERGY_RANDOM_WALK + ENERGY_RANDOM_WALK
        val result2 = sender.expectMsgType[UpdateInsect]
        assert(anthillInfo.position --> result2.info.position < anthillInfo.radius)
        assert(result2.info.inertia == result1.delta)
        assert(result2.info.energy == finalEnergy)
        sender expectNoMessage
      }

      "eat the reserve inside the anthill" in {
        ant ! Clock(3)
        val finalEnergy = startingEnergy + ENERGY_RANDOM_WALK + ENERGY_RANDOM_WALK + ENERGY_EATING + FOOD_ENERGY_CONVERSION * FOOD_EATEN_PER_STEP
        val result1 = sender.expectMsgType[UpdateInsect]
        assert(anthillInfo.position --> result1.info.position < anthillInfo.radius)
        assert(result1.info.inertia == ZeroVector2D())
        assert(result1.info.energy == finalEnergy)
        sender expectNoMessage
      }
    }

  }

}
