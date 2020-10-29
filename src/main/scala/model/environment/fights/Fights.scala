package model.environment.fights

import common.geometry.Vector2D
import model.insects.info.EnemyInfo

/** A fight between two entities.
  *
  * @param firstFighter  first entity
  * @param secondFighter second entity
  * @param position      of the fight
  * @tparam A first fighter type
  * @tparam B second fighter type
  */
case class Fight[A, B](firstFighter: A, secondFighter: B, position: Vector2D)

object Fights {

  /** Given a fight, returns loser and position of it
    *
    * @param fight a fight
    * @return loser in provided fight and its position
    */
  def loser[A: FightOutcome](fight: Fight[A, EnemyInfo]): (Either[A, EnemyInfo], Vector2D) =
    (implicitly[FightOutcome[A]].loser(fight.firstFighter, fight.secondFighter), fight.position)

  /** Given a collection of fights, returns losers of each
    *
    * @param fights collection of fights
    * @return losers among provided fights and their positions
    */
  def losers[A: FightOutcome](fights: Iterable[Fight[A, EnemyInfo]])
  : Seq[(Either[A, EnemyInfo], Vector2D)] =
    fights.map(f => (implicitly[FightOutcome[A]].loser(f.firstFighter, f.secondFighter), f.position)).toSeq


  implicit class InsectFightImplicits[A: FightOutcome](fight: Fight[A, EnemyInfo]) {
    def getLoser: (Either[A, EnemyInfo], Vector2D) = loser(fight)
  }

  implicit class InsectFights[A: FightOutcome](fights: Seq[Fight[A, EnemyInfo]]) {
    def getLosers: Seq[(Either[A, EnemyInfo], Vector2D)] = losers(fights)
  }

}
