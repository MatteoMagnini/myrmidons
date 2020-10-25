package model.environment

import common.geometry.Vector2D
import model.Drawable
import model.insects.info.{EnemyInfo, ForagingAntInfo, InsectInfo, PatrollingAntInfo}

object Fights {

  /** A fight between two entities.
    *
    * @param firstFighter  first entity
    * @param secondFighter second entity
    */
  case class Fight[A, B](firstFighter: A, secondFighter: B, position: Vector2D)

  /** A fight outcome: defines how to determine who lost the fight */
  trait FightOutcome[A, B] {
    def loser(firstFighter: A, secondFighter: B): Either[A, B]
  }

  /** Given a fight, returns loser of it
    *
    * @param fight a fight
    * @return loser in provided fight
    */
  def loser[A, B](fight: Fight[A, B])(implicit outcome: FightOutcome[A, B]): (Either[A, B], Vector2D) =
    (outcome.loser(fight.firstFighter, fight.secondFighter), fight.position)

  /** Given a collection of fights, returns losers of each
    *
    * @param fights collection of fights
    * @return losers among provided fights
    */
  def losers[A, B](fights: Iterable[Fight[A, B]])(implicit outcome: FightOutcome[A, B])
  : Seq[(Either[A, B], Vector2D)] = {
    fights.map(f => (outcome.loser(f.firstFighter, f.secondFighter), f.position)).toSeq
  }

  /** Dead insect in a fight */
  sealed trait DeadInsect extends Drawable {
    def insect: InsectInfo
  }

  case class DeadAnt(override val insect: InsectInfo, override val position: Vector2D) extends DeadInsect

  case class DeadEnemy(override val insect: EnemyInfo, override val position: Vector2D) extends DeadInsect

  object InsectFight {

    /** An implementation of [[model.environment.Fights.FightOutcome]], in the case of a fight between foraging ants and enemies. */
    implicit val foragingInsectFight: FightOutcome[ForagingAntInfo, EnemyInfo] =
      (firstFighter: ForagingAntInfo, _: EnemyInfo) => Left(firstFighter)


    /** An implementation of [[model.environment.Fights.FightOutcome]], in the case of a fight between patrolling ants and enemies. */
    implicit val patrollingInsectFight: FightOutcome[PatrollingAntInfo, EnemyInfo] =
      (firstFighter: PatrollingAntInfo, secondFighter: EnemyInfo) =>
        if (firstFighter.energy < secondFighter.energy) Left(firstFighter) else Right(secondFighter)
  }

}
