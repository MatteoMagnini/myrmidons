package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import model.insects.{Enemy, ForagingAnt}
import model.insects.info.{EnemyInfo, ForagingAntInfo}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Messages.{Context, KillAnt, Move, TakeFood}
import utility.geometry.ZeroVector2D

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
        sender.expectMsgType[KillAnt]
        sender expectNoMessage

      }
    }
  }

  "An ant competence" when {

    val foragingAntInfo = ForagingAntInfo(senderRef).updateFoodPosition(Some(ZeroVector2D()))
    val foragingAnt = system.actorOf(ForagingAnt(foragingAntInfo,senderRef), "insect-1")
    def defaultBehaviour(data: ForagingAntInfo): Receive = {case _ =>}
    foragingAnt ! Context(None)
    val context = sender.expectMsgType[Context].context.get

    "food is near" should {

      "grab it" in {

        val pickFoodCompetence = PickFood()
        assert(pickFoodCompetence.hasPriority(foragingAntInfo))
        pickFoodCompetence(context,senderRef,foragingAnt,foragingAntInfo,defaultBehaviour)
        sender.expectMsgType[TakeFood]
        sender expectNoMessage

      }
    }
  }

}
