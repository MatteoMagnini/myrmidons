package model.environment.fights

import common.geometry.Vector2D
import model.Drawable
import model.environment.fights.Fights.InsectFightImplicits._
import model.environment.fights.Fights.{Fight, _}
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}

/** Checker of fights.
  *
  * @param foragingFights   fights between foraging ants and enemies
  * @param patrollingFights fights between patrolling ants and enemies
  */
class FightsChecker(val foragingFights: Seq[Fight[ForagingAntInfo, EnemyInfo]],
                    val patrollingFights: Seq[Fight[PatrollingAntInfo, EnemyInfo]]) {

  /** Check losers of a collection of fights.
    *
    * @return collection of [[model.environment.fights.DeadAnt]]
    *         and collection of [[model.environment.fights.DeadEnemy]]
    */
  def checkFights: (Seq[DeadAnt], Seq[DeadEnemy]) = {
    def _checkFights[A <: InsectInfo](fights: Seq[(Either[A, EnemyInfo], Vector2D)]): (Seq[DeadAnt], Seq[DeadEnemy]) = {
      fights match {
        case h :: t => h._1.fold(ant => (DeadAnt(ant, h._2) +: _checkFights(t)._1, _checkFights(t)._2),
          enemy => (_checkFights(t)._1, DeadEnemy(enemy, h._2) +: _checkFights(t)._2))
        case _ => (Seq.empty, Seq.empty)
      }
    }

    _checkFights(foragingFights.getLosers ++ patrollingFights.getLosers)
  }
}

object FightsChecker {

  def apply(foragingAntsInfo: Iterable[ForagingAntInfo],
            patrollingAntInfo: Iterable[PatrollingAntInfo],
            enemiesInfo: Iterable[EnemyInfo]): FightsChecker =
    new FightsChecker(findFights(foragingAntsInfo, enemiesInfo).toSeq,
      findFights(patrollingAntInfo, enemiesInfo).toSeq)

  /** Given a collection of ants and a collection of enemies, find fights between them.
    *
    * @param antsInfo    ants collection
    * @param enemiesInfo enemies collection
    * @return fights between insects
    */
  private def findFights[A <: InsectInfo](antsInfo: Iterable[A],
                                          enemiesInfo: Iterable[EnemyInfo]): Iterable[Fight[A, EnemyInfo]] =
    for {
      ant <- antsInfo
      enemy <- enemiesInfo
      if ant.position ~~ enemy.position
    } yield Fight(ant, enemy, ant.position)

}

/** Dead insect in a fight */
sealed trait DeadInsect extends Drawable {
  def insect: InsectInfo
}

case class DeadAnt(override val insect: InsectInfo, override val position: Vector2D) extends DeadInsect

case class DeadEnemy(override val insect: EnemyInfo, override val position: Vector2D) extends DeadInsect

