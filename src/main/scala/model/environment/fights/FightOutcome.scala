package model.environment.fights

import model.insects.info.{EnemyInfo, ForagingAntInfo, PatrollingAntInfo}

/** A fight outcome: defines how to determine who lost the fight */
trait FightOutcome[A] {

  def loser(firstFighter: A, secondFighter: EnemyInfo): Either[A, EnemyInfo]
}

object InsectFightImplicits {

  /** An implementation of [[model.environment.fights.FightOutcome]],
    * in case of a fight between foraging ants and enemies. */
  implicit val foragingInsectFight: FightOutcome[ForagingAntInfo] =
    (firstFighter: ForagingAntInfo, _: EnemyInfo) => Left(firstFighter)


  /** An implementation of [[model.environment.fights.FightOutcome]],
    * in case of a fight between patrolling ants and enemies. */
  implicit val patrollingInsectFight: FightOutcome[PatrollingAntInfo] =
    (firstFighter: PatrollingAntInfo, secondFighter: EnemyInfo) =>
      if (firstFighter.energy < secondFighter.energy) Left(firstFighter) else Right(secondFighter)
}