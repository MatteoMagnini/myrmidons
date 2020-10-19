package model.environment.utility

import model.environment.Fights.InsectFight._
import model.environment.Fights.{Fight, _}
import model.insects.info.{EnemyInfo, InsectInfo}

/**
  *
  * @param fights
  */
class FightsChecker(val fights: Iterable[Fight[InsectInfo, EnemyInfo]]) {

  def checkFights: (Seq[InsectInfo], Seq[EnemyInfo]) = {
    def _checkFights(fights: Seq[Either[InsectInfo, EnemyInfo]]): (Seq[InsectInfo], Seq[EnemyInfo]) = {
      fights match {
        case h :: t => h.fold(x => (x +: _checkFights(t)._1, _checkFights(t)._2),
          y => (_checkFights(t)._1, y +: _checkFights(t)._2))
        case _ => (Seq.empty, Seq.empty)
      }
    }
    _checkFights(losers(fights))
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
