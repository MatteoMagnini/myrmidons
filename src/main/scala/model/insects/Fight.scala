package model.insects

object Fight {

  /** A fight outcome */
  trait FightOutcome[A] {
    def loser(x: A, y: A): A
  }

  /** Given a collection of fights, returns losers of each
    *
    * @param fighters collection of fighters
    * @return losers among provided fighters
    */
  def losers[T: FightOutcome](fighters: Iterable[(T, T)]): Iterable[T] = {
    fighters.map(x => implicitly[FightOutcome[T]].loser(x._1, x._2))
  }

  object InsectFight {

    /** An implementation of [[FightOutcome]], in the case of a fight between insects. */
    implicit val insectFight: FightOutcome[InsectInfo] = (x: InsectInfo, y: InsectInfo) => if (x.energy < y.energy) x else y
  }
}

