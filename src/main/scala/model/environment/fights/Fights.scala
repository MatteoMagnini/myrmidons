package model.environment.fights

import common.geometry.Vector2D

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
  def loser[A, B](fight: Fight[A, B])(implicit outcome: FightOutcome[A, B]): (Either[A, B], Vector2D) =
    (outcome.loser(fight.firstFighter, fight.secondFighter), fight.position)

  /** Given a collection of fights, returns losers of each
    *
    * @param fights collection of fights
    * @return losers among provided fights and their positions
    */
  def losers[A, B](fights: Iterable[Fight[A, B]])(implicit outcome: FightOutcome[A, B])
  : Seq[(Either[A, B], Vector2D)] =
    fights.map(f => (outcome.loser(f.firstFighter, f.secondFighter), f.position)).toSeq

  implicit class InsectFight[A, B](fight: Fight[A, B])(implicit outcome: FightOutcome[A, B]) {
    def getLoser: (Either[A, B], Vector2D) = loser(fight)
  }

  implicit class InsectFights[A, B](fights: Seq[Fight[A, B]])(implicit outcome: FightOutcome[A, B]) {
    def getLosers: Seq[(Either[A, B], Vector2D)] = losers(fights)
  }

}
