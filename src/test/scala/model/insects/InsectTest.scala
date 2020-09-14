package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry._
import utility.Messages._

class InsectTest extends TestKit(ActorSystem("InsectTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "Foraging ant info" when {

    "initialized" should {

      def checkAll(info: ForagingAntInfo,
                   position: Vector2D = ZeroVector2D(),
                   energy: Double = 100,
                   inertia: Vector2D = ZeroVector2D(),
                   pheromoneIsEmpty: Boolean = true,
                   proximityIsEmpty: Boolean = true, //Not tested now
                   foodAmount: Double = 0,
                   time: Int = 0,
                   id: Int = 0): Boolean = {

        info.position == position &&
          info.energy == energy &&
          info.inertia == inertia &&
          info.pheromoneSensor.entities.isEmpty == pheromoneIsEmpty &&
          info.proximitySensor.entities.isEmpty  == proximityIsEmpty &&
          info.foodAmount == foodAmount &&
          info.time == time &&
          info.id == id
      }

      val info1 = ForagingAntInfo(senderRef)

      "be correctly initialized" in {
        assert(checkAll(info1))
      }

      val newPosition = RandomVector2D(0.2,1.5)
      val info2 = info1.updatePosition(newPosition).asInstanceOf[ForagingAntInfo]

      "correct update position" in {
        assert(checkAll(info2, position = newPosition))
      }

      val delta = - 3
      val newEnergy = info2.energy + delta
      val info3 = info2.updateEnergy(delta).asInstanceOf[ForagingAntInfo]

      "correct update energy" in {
        assert(checkAll(info3, position = newPosition, energy = newEnergy))
      }

      val newInertia = RandomVector2D(0.2,1.5)
      val info4 = info3.updateInertia(newInertia).asInstanceOf[ForagingAntInfo]

      "correct update inertia" in {
        assert(checkAll(info4, position = newPosition, energy = newEnergy, inertia = newInertia))
      }

      val newPheromones = List(Entity(ZeroVector2D(),1))
      val pheromoneIsEmpty = false
      val info5 = info4.addPheromones(newPheromones)

      "correct update pheromones" in {
        assert(checkAll(info5, position = newPosition, energy = newEnergy, inertia = newInertia,
          pheromoneIsEmpty = pheromoneIsEmpty))
      }

      val newFood = 1
      val info6 = info5.incFood(newFood)

      "correct update food" in {
        assert(checkAll(info6, position = newPosition, energy = newEnergy, inertia = newInertia,
          pheromoneIsEmpty = pheromoneIsEmpty, foodAmount = newFood))
      }

      val newTime = 1
      val info7 = info6.incTime().asInstanceOf[ForagingAntInfo]

      "correct update time" in {
        assert(checkAll(info7, position = newPosition, energy = newEnergy, inertia = newInertia,
          pheromoneIsEmpty = pheromoneIsEmpty, foodAmount = newFood, time = newTime))
      }
    }
  }

  "Foraging Ant" must {

    val ant = system.actorOf(ForagingAnt(ForagingAntInfo(senderRef),senderRef), "ant-0")

    "perform random walk" in {
      ant ! Clock(1)
      val result1 = sender.expectMsgType[Move]
      ant ! NewPosition(result1.start >> result1.delta, result1.delta)
      val result2 = sender.expectMsgType[UpdateInsect]
      assert(result2.info.position != ZeroVector2D())
      assert(result2.info.energy == 99)
      sender expectNoMessage
    }

    "multiple times" in {
      ant ! Clock(2)
      val result1 = sender.expectMsgType[Move]
      ant ! NewPosition(result1.start >> result1.delta, result1.delta)
      val result2 = sender.expectMsgType[UpdateInsect]
      assert(result2.info.position != ZeroVector2D())
      assert(result2.info.energy == 98)
      sender expectNoMessage
    }

  }

  "Pheromone sensor" must {

    val info = ForagingAntInfo(senderRef)
    val pheromones = List(Entity(ZeroVector2D(), 1))

    "be updated" in {
      assert(info.pheromoneSensor.entities.isEmpty)
      val info2 = info.addPheromones(pheromones)
      assert(info2.pheromoneSensor.entities.nonEmpty)
    }
  }

  "Foraging Ant perceiving food pheromones" must {

    val ant = system.actorOf(ForagingAnt(ForagingAntInfo(senderRef, id = 1),senderRef), "ant-1")

    "perform food pheromone taxis" in {
      val pheromones = List(Entity(Vector2D(10,0),0.5))
      ant ! FoodPheromones(pheromones)
      ant ! Clock(1)
      val result1 = sender.expectMsgType[Move]
      ant ! NewPosition(result1.start >> result1.delta, result1.delta)
      val result2 = sender.expectMsgType[UpdateInsect]
      assert(result2.info.position == Vector2D(5,0))
      assert(result2.info.energy == 98.5)
      sender expectNoMessage
    }

    "multiple times" in {
      val pheromones = List(Entity(Vector2D(5,0),0.5))
      ant ! FoodPheromones(pheromones)
      ant ! Clock(2)
      val result1 = sender.expectMsgType[Move]
      ant ! NewPosition(result1.start >> result1.delta, result1.delta)
      val result2 = sender.expectMsgType[UpdateInsect]
      assert(result2.info.position == Vector2D(7.5,0))
      assert(result2.info.energy == 97)
      sender expectNoMessage
    }
  }

}
