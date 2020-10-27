package model.environment

import common.geometry.Vector2D
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}

object Fights {

  /** A fight between two entities.
    *
    * @param firstFighter  first entity
    * @param secondFighter second entity
    */
  case class Fight[A, B](firstFighter: A, secondFighter: B, position: Vector2D)

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


  /** A fight outcome: defines how to determine who lost the fight */
  trait FightOutcome[A, B] {
    def loser(firstFighter: A, secondFighter: B): Either[A, B]
  }

  object InsectFight {

    /** An implementation of [[model.environment.Fights.FightOutcome]],
      * in case of a fight between foraging ants and enemies. */
    implicit val foragingInsectFight: FightOutcome[ForagingAntInfo, EnemyInfo] =
      (firstFighter: ForagingAntInfo, _: EnemyInfo) => Left(firstFighter)


    /** An implementation of [[model.environment.Fights.FightOutcome]],
      * in case of a fight between patrolling ants and enemies. */
    implicit val patrollingInsectFight: FightOutcome[PatrollingAntInfo, EnemyInfo] =
      (firstFighter: PatrollingAntInfo, secondFighter: EnemyInfo) =>
        if (firstFighter.energy < secondFighter.energy) Left(firstFighter) else Right(secondFighter)
  }

}
