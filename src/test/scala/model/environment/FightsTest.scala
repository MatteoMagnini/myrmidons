package model.environment

import common.geometry.ZeroVector2D
import model.environment.Fights.InsectFight._
import model.environment.Fights.{Fight, loser, _}
import model.environment.utility.FightsChecker
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Random

class FightsTest extends AnyWordSpecLike with Matchers {

  "A fight between foraging ant and insect" when {
    val ant = ForagingAntInfo(null)
    val insect = EnemyInfo()
    val fight: Fight[InsectInfo, EnemyInfo] = Fight(ant, insect)

    "happens" should {

      "have as looser foraging ant" in {
        assert(loser(fight).fold(ant => true, enemy => false))
      }
    }
  }

  "A collection of fights between foraging ants and insects" when {

    val nFights = 5
    val fights: Iterable[Fight[InsectInfo, EnemyInfo]] = for {
      _ <- 0 to nFights
      ant: InsectInfo = ForagingAntInfo(null)
      enemy: EnemyInfo = EnemyInfo()
    } yield Fight(ant.asInstanceOf[InsectInfo], enemy)

    "happens" should {

      "have as losers foraging ants" in {
        val insectLosers = losers(fights)
        for (loser <- insectLosers) {
          assert(loser fold(
            ant => true,
            enemy => false
          ))
        }
      }
    }
  }

  "A fight between patrolling ants and insect" when {
    val antEnergy = 0
    val ant = PatrollingAntInfo(null, energy = antEnergy)
    val insectEnergy = 10
    val insect = EnemyInfo(energy = insectEnergy)
    val fight: Fight[InsectInfo, EnemyInfo] = Fight(ant, insect)

    "happens" should {

      "have as loser insect with lower energy" in {

        assert(loser(fight).fold(
          x => x.energy == (antEnergy min insectEnergy),
          y => y.energy == (antEnergy min insectEnergy)
        ))
      }
    }
  }

  "A collection of fights between patrolling ants and insects" when {

    val nFights = 5
    val maxEnergy = 100
    val fights: Iterable[Fight[InsectInfo, EnemyInfo]] = for {
      _ <- 0 to nFights
      antEnergy = Random.nextInt(maxEnergy)
      ant: InsectInfo = PatrollingAntInfo(null, energy = antEnergy)
      insectEnergy = Random.nextInt(maxEnergy)
      enemy: EnemyInfo = EnemyInfo(energy = insectEnergy)
    } yield Fight(ant.asInstanceOf[InsectInfo], enemy)

    "happens" should {

      "have as losers insects with lower energy in each fight" in {
        val minEnergies = fights.map(x => x.firstFighter.energy min x.secondFighter.energy)
        val insectLosers = losers(fights)
        for ((energy, loser) <- minEnergies zip insectLosers) {
          assert(loser fold(
            x => x.energy == energy,
            y => y.energy == energy
          ))
        }
      }
    }
  }

  "A foraging ant and an enemy" when {
    val ant = ForagingAntInfo(null, position = ZeroVector2D())
    val enemy = EnemyInfo(position = ZeroVector2D())

    "are in the same position" should {
      val fightsChecker = FightsChecker(Seq(ant), Seq(enemy))

      "begin a fight" in {
        assert(fightsChecker.fights.size == 1)
      }
      "have ant as loser" in {
        assert(fightsChecker.checkFights._1.size == 1)
        assert(fightsChecker.checkFights._2.isEmpty)
      }
    }
  }
}

