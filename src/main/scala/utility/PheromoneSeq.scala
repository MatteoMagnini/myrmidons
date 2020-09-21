package utility

import model.environment.Pheromone

object PheromoneSeq {

  implicit class PheromoneSeq[A <: Pheromone](seq: Seq[A]) {

    def tick(): Seq[A] =
      seq.toStream.map(p => p.decrease).filter(opt => opt.isDefined).map(opt => opt.get.asInstanceOf[A])

    def merge(threshold: Double): Seq[A] = ???

    private def recursiveMerge(): Seq[A] = ???



  }
}
