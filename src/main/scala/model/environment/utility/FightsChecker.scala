package model.environment.utility

import common.geometry.Vector2D
import model.environment.Fights.InsectFight._
import model.environment.Fights.{Fight, _}
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}

/** Checker of fights
  *
  * @param foragingFights   fights between foraging ants and enemies
  * @param patrollingFights fights between patrolling ants and enemies
  */
class FightsChecker(val foragingFights: Iterable[Fight[ForagingAntInfo, EnemyInfo]],
                    val patrollingFights: Iterable[Fight[PatrollingAntInfo, EnemyInfo]]) {

  /** Check losers of a collection of fights
    *
    * @return collection of looser ants and collection of loser enemies
    */
  def checkFights: (Seq[DeadAnt], Seq[DeadEnemy]) = {
    def _checkFights[A <: InsectInfo](fights: Seq[(Either[A, EnemyInfo], Vector2D)])
    : (Seq[DeadAnt], Seq[DeadEnemy]) = {
      fights match {
        case h :: t => h._1.fold(ant => (DeadAnt(ant, h._2) +: _checkFights(t)._1, _checkFights(t)._2),
          enemy => (_checkFights(t)._1, DeadEnemy(enemy, h._2) +: _checkFights(t)._2))
        case _ => (Seq.empty, Seq.empty)
      }
    }

    _checkFights(losers(foragingFights) ++ losers(patrollingFights))
  }
}

object FightsChecker {

  def apply(foragingAntsInfo: Iterable[ForagingAntInfo],
            patrollingAntInfo: Iterable[PatrollingAntInfo],
            enemiesInfo: Iterable[EnemyInfo]): FightsChecker =
    new FightsChecker(findFights(foragingAntsInfo, enemiesInfo),
      findFights(patrollingAntInfo, enemiesInfo))

  /** Given a collection of ants and a collection of enemies, find fights between them
    *
    * @param antsInfo    ants collection
    * @param enemiesInfo enemies collection
    * @return fights between insects
    */
  private def findFights[A <: InsectInfo](antsInfo: Iterable[A], enemiesInfo: Iterable[EnemyInfo])
  : Iterable[Fight[A, EnemyInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)

}
