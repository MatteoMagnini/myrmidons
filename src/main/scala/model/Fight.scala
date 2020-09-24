package model

import model.insects.InsectInfo
import utility.Geometry.Vector2D

object Fights {

  case class Fight[A](firstFighter: A, secondFighter: A, position: Vector2D) extends Drawable

  /** A fight outcome */
  trait FightOutcome[A] {
    def loser(firstFighter: A, secondFighter: A): A
  }

  /** Given a collection of fights, returns losers of each
    *
    * @param fighters collection of fighters
    * @return losers among provided fighters
    */
  def losers[T: FightOutcome](fighters: Iterable[Fight[T]]): Iterable[T] = {
    fighters.map(f => implicitly[FightOutcome[T]].loser(f.firstFighter, f.secondFighter))
  }

  def loser[T: FightOutcome](fighters: Fight[T]): T =
    implicitly[FightOutcome[T]].loser(fighters.firstFighter, fighters.secondFighter)

  object InsectFight {

    /** An implementation of [[FightOutcome]], in the case of a fight between insects. */
    implicit val insectFight: FightOutcome[InsectInfo] =
      (firstFighter: InsectInfo, secondFighter: InsectInfo) => if (firstFighter.energy < secondFighter.energy) firstFighter else secondFighter
  }
}

