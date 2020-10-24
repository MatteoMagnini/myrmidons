package view.actor

trait InfoReport {
  def clock: Int

  def foragingAntSize: Int

  def patrollingAntSize: Int

  def enemiesSize: Int

  def anthillFoodSize: Int
}

object InfoReport {
  def apply(clock: Int, foragingAntSize: Int, patrollingAntSize: Int,
            enemiesSize: Int, anthillFoodSize: Int): InfoReport =
    InfoReportImpl(clock, foragingAntSize,
      patrollingAntSize, enemiesSize, anthillFoodSize)

  /** Info into history list to be collect in json file.
   *
   * @param clock             current clock value.
   * @param foragingAntSize   current foraging ant size value.
   * @param patrollingAntSize current patrolling ant size value.
   * @param enemiesSize       current enemies size value.
   * @param anthillFoodSize   current anthill food size value.
   */
  private[view] case class InfoReportImpl(override val clock: Int,
                                          override val foragingAntSize: Int,
                                          override val patrollingAntSize: Int,
                                          override val enemiesSize: Int,
                                          override val anthillFoodSize: Int) extends InfoReport

}

