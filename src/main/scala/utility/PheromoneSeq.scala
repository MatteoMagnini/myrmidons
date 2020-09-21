package utility

import model.environment.Pheromone

object PheromoneSeq {

  implicit class PheromoneSeq[A <: Pheromone](seq: Seq[A]) {

    def tick(): Seq[A] =
      seq.toStream.map(p => p.decrease).filter(opt => opt.isDefined).map(opt => opt.get.asInstanceOf[A])

    def add(newElement: A, threshold: Double = 1E-10): Seq[A] =
      merge(newElement,threshold)

    private def merge(newElement: A, threshold: Double): Seq[A] =
      recursiveMerge(newElement, threshold, seq) match {
        case Some(x) => x +: seq
        case None => newElement +: seq
      }

    @scala.annotation.tailrec
    private def recursiveMerge(newElement: A, threshold: Double, sequence: Seq[A]): Option[A] =
      if (sequence.nonEmpty) {
        sequence.last.merge(newElement,threshold) match {
          case Some(x) => Some(x.asInstanceOf[A])
          case None => recursiveMerge(newElement, threshold, sequence.take(sequence.length - 1))
        }
      } else None

  }
}
