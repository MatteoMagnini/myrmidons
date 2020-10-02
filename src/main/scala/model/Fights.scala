package model


import utility.geometry.Vector2D
import model.insects.info.{EnemyInfo, ForagingAntInfo, SpecificInsectInfo}


object Fights {

  /** A fight between two entities
    *
    * @param firstFighter first entity
    * @param secondFighter second entity
    * @param position position where the fight takes place
    */
  case class Fight[A <: SpecificInsectInfo[A], B <: SpecificInsectInfo[B]](firstFighter: A, secondFighter: B, position: Vector2D) extends Drawable

  /** A fight outcome: defines how to determine who lost the fight */
  trait FightOutcome[A <: SpecificInsectInfo[A], B <: SpecificInsectInfo[B]] {
    def loser(firstFighter: A, secondFighter: B): Either[A, B]
  }

  /** Given a fight, returns loser of it
    *
    * @param fight a fight
    * @return loser in provided fight
    */
  def loser[A <: SpecificInsectInfo[A], B <: SpecificInsectInfo[B]](fight: Fight[A, B])(implicit outcome: FightOutcome[A, B]): Either[A, B] =
    outcome.loser(fight.firstFighter, fight.secondFighter)

  /** Given a collection of fights, returns losers of each
    *
    * @param fights collection of fights
    * @return losers among provided fights
    */
  def losers[A <: SpecificInsectInfo[A], B <: SpecificInsectInfo[B]](fights: Iterable[Fight[A, B]])(implicit outcome: FightOutcome[A, B]): Iterable[Either[A,B]] = {
    fights.map(f => outcome.loser(f.firstFighter, f.secondFighter))
  }

  object InsectFight {

    /** An implementation of [[FightOutcome]], in the case of a fight between insects. */
    implicit val insectFight: FightOutcome[ForagingAntInfo, EnemyInfo] =
      (firstFighter: ForagingAntInfo, secondFighter: EnemyInfo) =>
        if (firstFighter.energy < secondFighter.energy) Left(firstFighter) else Right(secondFighter)

    //TODO: add patrolling and other cases

  }
}

