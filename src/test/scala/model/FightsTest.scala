package model

import model.Fights.Fight
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.geometry.ZeroVector2D

import scala.util.Random


class FightsTest extends AnyWordSpecLike with Matchers {

  "A fight between insect" when {
    val antEnergy = 0
    val ant = PatrollingAntInfo(null, energy = antEnergy)
    val insectEnergy = 10
    val insect = EnemyInfo(energy = insectEnergy)
    val fight: Fight[InsectInfo, EnemyInfo] = Fight(ant, insect, ZeroVector2D())

    "happens" should {

      "have as loser insect with lower energy" in {
        import Fights._
        import Fights.InsectFight._

        assert(loser(fight) match {
          case Left(x) => x.energy == (antEnergy min insectEnergy)
          case Right(x) => x.energy == (antEnergy min insectEnergy)
        })
      }
    }
  }

  "A collection of fights between insects" when {
    import Fights._
    import Fights.InsectFight._

    val nFights = 5
    val maxEnergy = 100
    val fights: Iterable[Fight[InsectInfo, EnemyInfo]] = for {
      _ <- 0 to nFights
      antEnergy = Random.nextInt(maxEnergy)
      ant: InsectInfo = PatrollingAntInfo(null, energy = antEnergy)
      insectEnergy = Random.nextInt(maxEnergy)
      enemy: EnemyInfo = EnemyInfo(energy = insectEnergy)
    } yield Fight(ant, enemy, ZeroVector2D())

    "happens" should {

      "have as losers insects with lower energy in each fight" in {
        val minEnergies = fights.map(x => x.firstFighter.energy min x.secondFighter.energy)
        val insectLosers = losers(fights)
        for ((energy, loser) <- minEnergies zip insectLosers) {
          assert(loser match {
            case Left(x) => x.energy == energy
            case Right(x) => x.energy == energy
          })
        }
      }
    }
  }

}
