package model.insects

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.environment.pheromones.FoodPheromone
import model.insects.info.ForagingAntInfo
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import common.geometry._

class InsectInfoTest extends TestKit(ActorSystem("InsectInfoTest"))
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "Foraging ant info" when {

    val DELTA = 1E-10
    val startingIntensity = 100.0

    "initialized" should {

      def checkAll(info: ForagingAntInfo,
                   position: Vector2D = ZeroVector2D(),
                   isInsideTheAnthill: Boolean = false,
                   energy: Double = 100,
                   inertia: Vector2D = ZeroVector2D(),
                   pheromoneIsEmpty: Boolean = true,
                   proximityIsEmpty: Boolean = true, //Not tested now
                   foodAmount: Double = 0,
                   time: Int = 0,
                   id: Int = 0): Boolean = {

        info.position == position &&
          info.isInsideTheAnthill == isInsideTheAnthill &&
          info.energy == energy &&
          info.inertia == inertia &&
          info.foodPheromones.isEmpty == pheromoneIsEmpty &&
          //info.proximitySensor.entities.isEmpty  == proximityIsEmpty &&
          info.foodAmount == foodAmount &&
          info.time == time &&
          info.id == id
      }

      val info1 = ForagingAntInfo(senderRef)

      "be correctly initialized" in {
        assert(checkAll(info1))
      }

      val newPosition = RandomVector2DInSquare(0.2,1.5)
      val info2 = info1.updatePosition(newPosition)

      "correct update position" in {
        assert(checkAll(info2, position = newPosition))
      }

      val info3 = info2.updateAnthillCondition(true)

      "correct update anthill condition" in {
        assert(checkAll(info3, position = newPosition, isInsideTheAnthill = true))
      }

      val delta = - 3
      val newEnergy = info2.energy + delta
      val info4 = info2.updateEnergy(delta)

      "correct update energy" in {
        assert(checkAll(info4, position = newPosition, energy = newEnergy))
      }

      val newInertia = RandomVector2DInSquare(0.2,1.5)
      val info5 = info4.updateInertia(newInertia)

      "correct update inertia" in {
        assert(checkAll(info5, position = newPosition, energy = newEnergy, inertia = newInertia))
      }

      val newPheromones = Seq(FoodPheromone(ZeroVector2D(), x => x - DELTA, startingIntensity))
      val pheromoneIsEmpty = false
      val info6 = info5.updateFoodPheromones(newPheromones)

      "correct update pheromones" in {
        assert(checkAll(info6, position = newPosition, energy = newEnergy, inertia = newInertia,
          pheromoneIsEmpty = pheromoneIsEmpty))
      }

      val newFood = 1
      val info7 = info6.incFood(newFood)

      "correct update food" in {
        assert(checkAll(info7, position = newPosition, energy = newEnergy, inertia = newInertia,
          pheromoneIsEmpty = pheromoneIsEmpty, foodAmount = newFood))
      }

      val newTime = 1
      val info8 = info7.incTime()

      "correct update time" in {
        assert(checkAll(info8, position = newPosition, energy = newEnergy, inertia = newInertia,
          pheromoneIsEmpty = pheromoneIsEmpty, foodAmount = newFood, time = newTime))
      }
    }
  }

}
