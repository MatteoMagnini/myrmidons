package model.environment

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import common.geometry.Vector2DFactory.ZeroVector2D
import model.environment.anthill.{Anthill, AnthillInfo}
import model.environment.fights.Fights._
import model.environment.fights.InsectFightImplicits._
import model.environment.fights.{Fight, FightsChecker}
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Random

class FightsTest  extends TestKit(ActorSystem("FightsTest"))
  with AnyWordSpecLike
  with Matchers {

  val sender: TestProbe = TestProbe()
  val anthill: ActorRef = system.actorOf(Anthill(AnthillInfo(ZeroVector2D()), sender.ref), name = "anthill")

  "A fight between foraging ant and insect" when {
    val ant = ForagingAntInfo(anthill)
    val insect = EnemyInfo()
    val fight: Fight[ForagingAntInfo, EnemyInfo] = Fight(ant, insect, ZeroVector2D())

    "happens" should {

      "have as looser foraging ant" in {
        assert(fight.getLoser._1.fold(_ => true, _ => false))
      }
    }
  }

  "A collection of fights between foraging ants and insects" when {

    val nFights = 5
    val fights: Seq[Fight[ForagingAntInfo, EnemyInfo]] = for {
      _ <- 0 to nFights
      ant = ForagingAntInfo(anthill)
      enemy: EnemyInfo = EnemyInfo()
    } yield Fight(ant, enemy, ZeroVector2D())

    "happens" should {

      "have as losers foraging ants" in {
        val insectLosers = fights.getLosers.map(_._1)
        for (loser <- insectLosers) {
          assert(loser fold(_ => true, _ => false))
        }
      }
    }
  }

  "A fight between patrolling ants and insect" when {
    val antEnergy = 0
    val ant = PatrollingAntInfo(anthill, energy = antEnergy)
    val insectEnergy = 10
    val insect = EnemyInfo(energy = insectEnergy)
    val fight: Fight[PatrollingAntInfo, EnemyInfo] = Fight(ant, insect, ZeroVector2D())

    "happens" should {

      "have as loser insect with lower energy" in {

        assert(fight.getLoser._1.fold(
          x => x.energy == (antEnergy min insectEnergy),
          y => y.energy == (antEnergy min insectEnergy)
        ))
      }
    }
  }

  "A collection of fights between patrolling ants and insects" when {

    val nFights = 5
    val maxEnergy = 100
    val fights: Seq[Fight[PatrollingAntInfo, EnemyInfo]] = for {
      _ <- 0 to nFights
      antEnergy = Random.nextInt(maxEnergy)
      ant = PatrollingAntInfo(anthill, energy = antEnergy)
      insectEnergy = Random.nextInt(maxEnergy)
      enemy = EnemyInfo(energy = insectEnergy)
    } yield Fight(ant, enemy, ZeroVector2D())

    "happens" should {

      "have as losers insects with lower energy in each fight" in {
        val minEnergies = fights.map(x => x.firstFighter.energy min x.secondFighter.energy)
        val insectLosers = fights.getLosers.map(_._1)
        for ((energy, loser) <- minEnergies zip insectLosers) {
          assert(loser fold(
            x => x.energy == energy,
            y => y.energy == energy
          ))
        }
      }
    }
  }

  "Ants and an enemy" when {
    val foragingAnt = ForagingAntInfo(anthill, position = ZeroVector2D())
    val patrollingAnt = PatrollingAntInfo(anthill, position = ZeroVector2D(), energy = 10)
    val enemy = EnemyInfo(position = ZeroVector2D(), energy = 20)

    "are in the same position" should {
      val fightsChecker = FightsChecker(Seq(foragingAnt), Seq(patrollingAnt), Seq(enemy))

      "begin fights" in {
        assert(fightsChecker.foragingFights.size == 1)
        assert(fightsChecker.patrollingFights.size == 1)
      }
      "have correct losers" in {
        assert(fightsChecker.checkFights._1.size == 2)
        assert(fightsChecker.checkFights._2.isEmpty)
      }
    }
  }
}

