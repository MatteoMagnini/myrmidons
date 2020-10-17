package model.environment

import common.geometry.Vector2D
import model.Drawable
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}

object Fights {

  /** A fight between two entities
    *
    * @param firstFighter first entity
    * @param secondFighter second entity
    * @param position position where the fight takes place
    */
  case class Fight[A, B](firstFighter: A, secondFighter: B, position: Vector2D) extends Drawable

  /** A fight outcome: defines how to determine who lost the fight */
  trait FightOutcome[A, B] {
    def loser(firstFighter: A, secondFighter: B): Either[A, B]
  }

  /** Given a fight, returns loser of it
    *
    * @param fight a fight
    * @return loser in provided fight
    */
  def loser[A, B](fight: Fight[A, B])(implicit outcome: FightOutcome[A, B]): Either[A, B] =
    outcome.loser(fight.firstFighter, fight.secondFighter)

  /** Given a collection of fights, returns losers of each
    *
    * @param fights collection of fights
    * @return losers among provided fights
    */
  def losers[A, B](fights: Iterable[Fight[A, B]])(implicit outcome: FightOutcome[A, B]): Seq[Either[A,B]] = {
    fights.map(f => outcome.loser(f.firstFighter, f.secondFighter)).toSeq
  }

  object InsectFight {

    /** An implementation of [[FightOutcome]], in the case of a fight between insects. */
    implicit val foragingInsectFight: FightOutcome[InsectInfo, EnemyInfo] =
      (firstFighter: InsectInfo, secondFighter: EnemyInfo) => firstFighter match {
        case p: PatrollingAntInfo => if (p.energy < secondFighter.energy) Left(p) else Right(secondFighter)
        case f: ForagingAntInfo => Left(f)
      }
  }
}
