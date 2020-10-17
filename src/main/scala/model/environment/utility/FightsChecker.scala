package model.environment.utility

import model.environment.Fights
import model.environment.Fights.Fight
import model.insects.info.{EnemyInfo, InsectInfo}

class FightsChecker(val fights: Iterable[Fight[InsectInfo, EnemyInfo]]) {

  def checkFights: (Seq[InsectInfo], Seq[EnemyInfo]) = {
    import Fights.InsectFight._
    import Fights._
    var ants:Seq[InsectInfo] = Seq.empty
    var enemies:Seq[EnemyInfo] = Seq.empty
    // TODO: other refactoring: try method fold
    losers(fights).foreach {
      case Left(ant) => ants = ants :+ ant
      case Right(enemy) => enemies = enemies :+ enemy
    }
    (ants, enemies)
  }
}

object FightsChecker {

  def apply(antsInfo: Iterable[InsectInfo], enemiesInfo: Iterable[EnemyInfo]): FightsChecker =
    new FightsChecker(findFights(antsInfo, enemiesInfo))

  private def findFights(antsInfo: Iterable[InsectInfo], enemiesInfo: Iterable[EnemyInfo])
  : Iterable[Fight[InsectInfo, EnemyInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)

}
