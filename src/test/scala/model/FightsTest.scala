package model

import model.Fights.Fight
import model.insects.{EnemyInfo, ForagingAntInfo, InsectInfo}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry.ZeroVector2D

import scala.util.Random

class FightsTest extends AnyWordSpecLike with Matchers {

  "A fight between insect" when {
    val antEnergy = 0
    val ant = ForagingAntInfo(null, energy = antEnergy)
    val insectEnergy = 10
    val insect = EnemyInfo(null, energy = insectEnergy)
    val fight: Fight[InsectInfo] = Fight(ant, insect, ZeroVector2D())

    "happens" should {

      "have as loser insect with lower energy" in {
        import Fights._
        import Fights.InsectFight._

        assert(loser(fight).energy == (antEnergy min insectEnergy))
      }
    }
  }

  "A collection of fights between insects" when {
    import Fights._
    import Fights.InsectFight._

    val nFights = 5
    val maxEnergy = 100
    val fights: Iterable[Fight[InsectInfo]] = for {
      _ <- 0 to nFights
      antEnergy = Random.nextInt(maxEnergy)
      ant: InsectInfo = ForagingAntInfo(null, energy = antEnergy)
      insectEnergy = Random.nextInt(maxEnergy)
      enemy: InsectInfo = EnemyInfo(null, energy = insectEnergy)
    } yield Fight(ant, enemy, ZeroVector2D())

    "happens" should {

      "have as losers insects with lower energy in each fight" in {
        val minEnergies = fights.map(x => x.firstFighter.energy min x.secondFighter.energy)
        val insectLosers = losers(fights)
        for ((energy, loser) <- minEnergies zip insectLosers) {
          assert(energy == loser.energy)
        }
      }
    }
  }
}
