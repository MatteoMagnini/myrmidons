package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.environment.FoodPheromone
import model.insects.{Enemy, ForagingAnt}
import model.insects.info.{EnemyInfo, ForagingAntInfo}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Messages.{AddFoodPheromone, AntTowardsAnthill, Context, EatFood, KillInsect, Move, StoreFood, TakeFood, UpdateInsect}
import utility.geometry.{Vector2D, ZeroVector2D}

class InsectCompetencesTest extends TestKit(ActorSystem("InsectCompetencesTest"))
with AnyWordSpecLike
with BeforeAndAfterAll{

  override def afterAll: Unit = {
  TestKit.shutdownActorSystem(system)
}

  val sender: TestProbe = TestProbe()
  implicit val senderRef: ActorRef = sender.ref

  "An insect competence" when {

    val insectInfo = EnemyInfo(energy = 0)
    val insect = system.actorOf(Enemy(insectInfo,senderRef), "insect-0")
    def defaultBehaviour(data: EnemyInfo): Receive = {case _ =>}
    insect ! Context(None)
    val context = sender.expectMsgType[Context].context.get

    "performing random walk" should {

      "should move" in {

        val randomWalkCompetence = RandomWalk[EnemyInfo]()
        assert(randomWalkCompetence.hasPriority(insectInfo))
        randomWalkCompetence(context,senderRef,insect,insectInfo,defaultBehaviour)
        sender.expectMsgType[Move]
        sender expectNoMessage

      }
    }

    "has zero energy" should {

      "die" in {

        val dieCompetence = Die[EnemyInfo]()
        assert(dieCompetence.hasPriority(insectInfo))
        dieCompetence(context,senderRef,insect,insectInfo,defaultBehaviour)
        sender.expectMsgType[KillInsect]
        sender expectNoMessage

      }
    }
  }

  "A foraging ant competence" when {

    val foragingAntInfo = ForagingAntInfo(senderRef).updateFoodPosition(Some(ZeroVector2D()))
    val foragingAnt = system.actorOf(ForagingAnt(foragingAntInfo,senderRef), "insect-1")
    def defaultBehaviour(data: ForagingAntInfo): Receive = {case _ =>}
    foragingAnt ! Context(None)
    val context = sender.expectMsgType[Context].context.get

    "food is near" should {

      "grab it" in {

        val pickFoodCompetence = PickFood()
        assert(pickFoodCompetence.hasPriority(foragingAntInfo))
        pickFoodCompetence(context, senderRef, foragingAnt, foragingAntInfo, defaultBehaviour)
        sender.expectMsgType[TakeFood]
        sender expectNoMessage

      }


      val foragingAntInfo2 = foragingAntInfo.incFood(utility.Parameters.ForagingAnt.MAX_FOOD)
      //Indirectly testing GoBackToHome because it is called in carryFoodCompetence apply.
      "carry to home" in {

        val carryFoodCompetence = CarryFoodToHome()
        assert(carryFoodCompetence.hasPriority(foragingAntInfo2))
        carryFoodCompetence(context, senderRef, foragingAnt, foragingAntInfo2, defaultBehaviour)
        sender.expectMsgType[AntTowardsAnthill]
        sender expectNoMessage

      }

      "realising food pheromones" in {

        val dropFoodPheromoneCompetence = DropFoodPheromone()
        //No assert on hasPriority because it is stochastic.
        dropFoodPheromoneCompetence(context, senderRef, foragingAnt, foragingAntInfo2, defaultBehaviour)
        sender.expectMsgType[AddFoodPheromone]
        sender.expectMsgType[UpdateInsect]
        sender expectNoMessage

      }

      val foragingAntInfo3 = foragingAntInfo2.updateAnthillCondition(true)
      "store to anthill" in {

        val storeFoodInAnthillCompetence = StoreFoodInAnthill()
        assert(storeFoodInAnthillCompetence.hasPriority(foragingAntInfo3))
        storeFoodInAnthillCompetence(context, senderRef, foragingAnt, foragingAntInfo3, defaultBehaviour)
        sender.expectMsgType[StoreFood]
        sender.expectMsgType[UpdateInsect]
        sender expectNoMessage

      }
    }

    //From now on competences are for all kind of ants (except from FoodPheromoneTaxis).
    "energy is low" should {

      val foragingAntInfo2 = foragingAntInfo.updateEnergy(-80).updateAnthillCondition(true)
      "eat from the anthill" in {

        val eatFromTheAnthillCompetence = EatFromTheAnthill[ForagingAntInfo]()
        assert(eatFromTheAnthillCompetence.hasPriority(foragingAntInfo2))
        eatFromTheAnthillCompetence(context,senderRef,foragingAnt,foragingAntInfo2,defaultBehaviour)
        sender.expectMsgType[EatFood]
        sender expectNoMessage

      }

      val foragingAntInfo3 = foragingAntInfo2.updateEnergy(80)
      "exit from the anthill" in {

        val goOutsideCompetence = GoOutside[ForagingAntInfo]()
        assert(goOutsideCompetence.hasPriority(foragingAntInfo3))
        goOutsideCompetence(context,senderRef,foragingAnt,foragingAntInfo3,defaultBehaviour)
        sender.expectMsgType[Move]
        sender expectNoMessage

      }

      val foragingAntInfo4 = foragingAntInfo3.updateFoodPheromones(Seq(FoodPheromone(Vector2D(2,3),0,10)))
      "follow the food pheromones" in {

        val foodPheromoneTaxisCompetence = FoodPheromoneTaxis()
        assert(foodPheromoneTaxisCompetence.hasPriority(foragingAntInfo4))
        foodPheromoneTaxisCompetence(context,senderRef,foragingAnt,foragingAntInfo3,defaultBehaviour)
        sender.expectMsgType[Move]
        sender expectNoMessage
      }
    }
  }

}
