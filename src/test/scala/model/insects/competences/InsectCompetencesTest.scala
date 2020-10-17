package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.environment.pheromones.{DangerPheromone, FoodPheromone}
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}
import model.insects.{Enemy, ForagingAnt, PatrollingAnt}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike
import common.Messages._
import common.geometry.{Vector2D, ZeroVector2D}

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

        val randomWalkCompetence = RandomWalk[EnemyInfo](defaultBehaviour)
        assert(randomWalkCompetence.hasPriority(insectInfo))
        randomWalkCompetence(context,senderRef,insect,insectInfo)
        sender.expectMsgType[Move]
        sender expectNoMessage

      }
    }

    "has zero energy" should {

      "die" in {

        val dieCompetence = Die[EnemyInfo](defaultBehaviour)
        assert(dieCompetence.hasPriority(insectInfo))
        dieCompetence(context,senderRef,insect,insectInfo)
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

        val pickFoodCompetence = PickFood(defaultBehaviour)
        assert(pickFoodCompetence.hasPriority(foragingAntInfo))
        pickFoodCompetence(context, senderRef, foragingAnt, foragingAntInfo)
        sender.expectMsgType[TakeFood]
        sender expectNoMessage

      }

      import model.insects.Ants.ForagingAnt._
      val foragingAntInfo2 = foragingAntInfo.incFood(MAX_FOOD)
      //Indirectly testing GoBackToHome because it is called in carryFoodCompetence apply.
      "carry to home" in {

        val carryFoodCompetence = CarryFoodToHome(defaultBehaviour)
        assert(carryFoodCompetence.hasPriority(foragingAntInfo2))
        carryFoodCompetence(context, senderRef, foragingAnt, foragingAntInfo2)
        sender.expectMsgType[AntTowardsAnthill]
        sender expectNoMessage

      }

      "realising food pheromones" in {

        val dropFoodPheromoneCompetence = DropFoodPheromone(defaultBehaviour)
        //No assert on hasPriority because it is stochastic.
        dropFoodPheromoneCompetence(context, senderRef, foragingAnt, foragingAntInfo2)
        sender.expectMsgType[AddPheromone]
        sender.expectMsgType[UpdateInsect]
        sender expectNoMessage

      }

      val foragingAntInfo3 = foragingAntInfo2.updateAnthillCondition(true)
      "store to anthill" in {

        val storeFoodInAnthillCompetence = StoreFoodInAnthill(defaultBehaviour)
        assert(storeFoodInAnthillCompetence.hasPriority(foragingAntInfo3))
        storeFoodInAnthillCompetence(context, senderRef, foragingAnt, foragingAntInfo3)
        sender.expectMsgType[StoreFood]
        sender.expectMsgType[UpdateInsect]
        sender expectNoMessage

      }
    }

    //From now on competences are for all kind of ants (except from FoodPheromoneTaxis).
    "energy is low" should {

      val foragingAntInfo2 = foragingAntInfo.updateEnergy(-80).updateAnthillCondition(true)
      "eat from the anthill" in {

        val eatFromTheAnthillCompetence = EatFromTheAnthill[ForagingAntInfo](defaultBehaviour)
        assert(eatFromTheAnthillCompetence.hasPriority(foragingAntInfo2))
        eatFromTheAnthillCompetence(context,senderRef,foragingAnt,foragingAntInfo2)
        sender.expectMsgType[EatFood]
        sender expectNoMessage

      }

      val foragingAntInfo3 = foragingAntInfo2.updateEnergy(80)
      "exit from the anthill" in {

        //No assert on hasPriority because it is stochastic.
        val goOutsideCompetence = GoOutside[ForagingAntInfo](defaultBehaviour)
        goOutsideCompetence(context,senderRef,foragingAnt,foragingAntInfo3)
        sender.expectMsgType[Move]
        sender expectNoMessage
      }

      import model.environment.pheromones.FoodPheromoneInfo._
      val foodPheromone = FoodPheromone(Vector2D(4,6),x => x - DELTA,10)
      val foragingAntInfo4 = foragingAntInfo3.updateFoodPheromones(Seq(foodPheromone)).updateAnthillCondition(false)
      "follow the food pheromones" in {

        val foodPheromoneTaxisCompetence = FoodPheromoneTaxis(defaultBehaviour)
        assert(foodPheromoneTaxisCompetence.hasPriority(foragingAntInfo4))
        foodPheromoneTaxisCompetence(context,senderRef,foragingAnt,foragingAntInfo3)
        val result = sender.expectMsgType[Move]
        assert((result.start >> result.delta) --> foodPheromone.position <
          foragingAntInfo4.position --> foodPheromone.position)
        sender expectNoMessage
      }
    }
  }

  "A patrolling ant competence" when {

    import model.environment.pheromones.DangerPheromoneInfo._
    val dangerPheromone = DangerPheromone(Vector2D(-7,2), x => x - DELTA, 10)
    def defaultBehaviour(data: PatrollingAntInfo): Receive = {case _ =>}
    val patrollingAntInfo = PatrollingAntInfo(senderRef)
    val patrollingAnt = system.actorOf(PatrollingAnt(patrollingAntInfo,senderRef), "insect-2")
    patrollingAnt ! Context(None)
    val context = sender.expectMsgType[Context].context.get

    "perceiving danger pheromones" should {

      val patrollingAntInfo = PatrollingAntInfo(senderRef).updateDangerPheromones(Seq(dangerPheromone))
      "move towards them" in {

        val dangerPheromoneTaxis = DangerPheromoneTaxis(defaultBehaviour)
        assert(dangerPheromoneTaxis.hasPriority(patrollingAntInfo))
        dangerPheromoneTaxis(context,senderRef,patrollingAnt,patrollingAntInfo)
        val result = sender.expectMsgType[Move]
        assert((result.start >> result.delta) --> dangerPheromone.position <
          patrollingAntInfo.position --> dangerPheromone.position)
        sender expectNoMessage
      }
    }
  }

}
