package model

import model.insects.InsectInfo
import utility.Geometry.Vector2D

object Fights {

  /** A fight between two entities
    *
    * @param firstFighter first entity
    * @param secondFighter second entity
    * @param position position where the fight takes place
    */
  case class Fight[A](firstFighter: A, secondFighter: A, position: Vector2D) extends Drawable

  /** A fight outcome: defines how to determine who lost the fight */
  trait FightOutcome[A] {
    def loser(firstFighter: A, secondFighter: A): A
  }

  /** Given a fight, returns loser of it
    *
    * @param fight a fight
    * @return loser in provided fight
    */
  def loser[T: FightOutcome](fight: Fight[T]): T =
    implicitly[FightOutcome[T]].loser(fight.firstFighter, fight.secondFighter)

  /** Given a collection of fights, returns losers of each
    *
    * @param fights collection of fights
    * @return losers among provided fights
    */
  def losers[T: FightOutcome](fights: Iterable[Fight[T]]): Iterable[T] = {
    fights.map(f => implicitly[FightOutcome[T]].loser(f.firstFighter, f.secondFighter))
  }

  object InsectFight {

    /** An implementation of [[FightOutcome]], in the case of a fight between insects. */
    implicit val insectFight: FightOutcome[InsectInfo] =
      (firstFighter: InsectInfo, secondFighter: InsectInfo) => if (firstFighter.energy < secondFighter.energy) firstFighter else secondFighter
  }
}

