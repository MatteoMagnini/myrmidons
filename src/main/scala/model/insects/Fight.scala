package model.insects

trait Fight[A] {

  def fighters: (A, A)
  def looser: A

}

object InsectFight {

  def apply(fighters: (InsectInfo, InsectInfo)): Fight[InsectInfo] = new FightImpl(fighters)

  private[this] class FightImpl(override val fighters: (InsectInfo, InsectInfo)) extends Fight[InsectInfo] {

    override def looser: InsectInfo = if (fighters._1.energy > fighters._2.energy) fighters._2 else fighters._1
  }
}