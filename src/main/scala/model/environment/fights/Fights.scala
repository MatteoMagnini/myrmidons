package model.environment.fights

import common.geometry.Vector2D
import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}

object Fights {

  /** A fight between two entities.
    *
    * @param firstFighter first entity
    * @param secondFighter second entity
    * @param position of the fight
    * @tparam A first fighter type
    * @tparam B second fighter type
    */
  case class Fight[A,B](firstFighter: A, secondFighter: B, position: Vector2D)

  /** A fight outcome: defines how to determine who lost the fight */
  trait InsectFightOutcome[A] {
    def loser(firstFighter: A, secondFighter: EnemyInfo): Either[A, EnemyInfo]
  }

  /** Given a fight, returns loser and position of it
    *
    * @param fight a fight
    * @return loser in provided fight and its position
    */
  def loser[A:InsectFightOutcome](fight: Fight[A, EnemyInfo]): (Either[A, EnemyInfo], Vector2D) =
    (implicitly[InsectFightOutcome[A]].loser(fight.firstFighter, fight.secondFighter), fight.position)

  /** Given a collection of fights, returns losers of each
    *
    * @param fights collection of fights
    * @return losers among provided fights and their positions
    */
  def losers[A:InsectFightOutcome](fights: Iterable[Fight[A, EnemyInfo]])
  : Seq[(Either[A, EnemyInfo], Vector2D)] =
    fights.map(f => (implicitly[InsectFightOutcome[A]].loser(f.firstFighter, f.secondFighter), f.position)).toSeq


  implicit class InsectFightImplicits[A: InsectFightOutcome](fight: Fight[A, EnemyInfo]) {
    def getLoser: (Either[A,EnemyInfo],Vector2D) = loser(fight)
  }

  implicit class InsectFights[A: InsectFightOutcome](fights: Seq[Fight[A, EnemyInfo]]) {
    def getLosers: Seq[(Either[A, EnemyInfo], Vector2D)] = losers(fights)
  }

  object InsectFightImplicits {

    /** An implementation of [[model.environment.fights.Fights.InsectFightOutcome]],
      * in case of a fight between foraging ants and enemies. */
    implicit val foragingInsectFight: InsectFightOutcome[ForagingAntInfo] =
      (firstFighter: ForagingAntInfo, _: EnemyInfo) => Left(firstFighter)


    /** An implementation of [[model.environment.fights.Fights.InsectFightOutcome]],
      * in case of a fight between patrolling ants and enemies. */
    implicit val patrollingInsectFight: InsectFightOutcome[PatrollingAntInfo] =
      (firstFighter: PatrollingAntInfo, secondFighter: EnemyInfo) =>
        if (firstFighter.energy < secondFighter.energy) Left(firstFighter) else Right(secondFighter)
  }
}
