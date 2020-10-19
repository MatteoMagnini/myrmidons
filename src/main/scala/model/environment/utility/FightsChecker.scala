package model.environment.utility

import model.environment.Fights.InsectFight._
import model.environment.Fights.{Fight, _}
import model.insects.info.{EnemyInfo, InsectInfo}

/** Checker of fights
  *
  * @param fights fights to deal with
  */
class FightsChecker(val fights: Iterable[InsectsFight]) {

  /** Check losers of a collection of fights
    *
    * @return collection of looser ants and collection of loser enemies
    */
  def checkFights: (Seq[InsectInfo], Seq[EnemyInfo]) = {
    def _checkFights(fights: Seq[Either[InsectInfo, EnemyInfo]]): (Seq[InsectInfo], Seq[EnemyInfo]) = {
      fights match {
        case h :: t => h.fold(ant => (ant +: _checkFights(t)._1, _checkFights(t)._2),
          enemy => (_checkFights(t)._1, enemy +: _checkFights(t)._2))
        case _ => (Seq.empty, Seq.empty)
      }
    }
    _checkFights(losers(fights.map(_.fight)))
  }
}

object FightsChecker {

  def apply(antsInfo: Iterable[InsectInfo], enemiesInfo: Iterable[EnemyInfo]): FightsChecker =
    new FightsChecker(findFights(antsInfo, enemiesInfo))

  /** Given a collection of ants and a collection of enemies, find fights between them
    *
    * @param antsInfo ants collection
    * @param enemiesInfo enemies collection
    * @return fights between insects
    */
  private def findFights(antsInfo: Iterable[InsectInfo], enemiesInfo: Iterable[EnemyInfo])
  : Iterable[InsectsFight] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield InsectsFight(Fight(ant, enemy), ant.position)

}
