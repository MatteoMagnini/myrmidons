package model.environment.fights

import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}

/** A fight outcome: defines how to determine who lost the fight */
trait FightOutcome[A,B] {

  def loser(firstFighter: A, secondFighter: B): Either[A, B]
}

object InsectFightImplicits {

  /** An implementation of [[model.environment.fights.FightOutcome]],
    * in case of a fight between foraging ants and enemies. */
  implicit val foragingInsectFight: FightOutcome[ForagingAntInfo, EnemyInfo] =
    (firstFighter: ForagingAntInfo, _: EnemyInfo) => Left(firstFighter)


  /** An implementation of [[model.environment.fights.FightOutcome]],
    * in case of a fight between patrolling ants and enemies. */
  implicit val patrollingInsectFight: FightOutcome[PatrollingAntInfo, EnemyInfo] =
    (firstFighter: PatrollingAntInfo, secondFighter: EnemyInfo) =>
      if (firstFighter.energy < secondFighter.energy) Left(firstFighter) else Right(secondFighter)
}