package common

import model.environment.pheromones.Pheromone

/**
 * Pimp my library pattern.
 * A rich map for [[Pheromone]] objects.
 */
object PheromoneMap {

  implicit class PheromoneMap[A <: Pheromone](map: Map[Int, A]) {

    /**
     * Call decrease function over all [[Pheromone]] instances.
     *
     * @return the new map with updated pheromones
     */
    def tick(): Map[Int, A] =
      map.toStream.map(m => m._1 -> m._2.decrease)
        .filter(opt => opt._2.isDefined)
        .map(opt => opt._1 -> opt._2.get.asInstanceOf[A]).toMap

    /**
     * @param newElement to be added
     * @param threshold  under which merge
     * @return the new map
     */
    def add(newElement: A, threshold: Double = 1E-10): Map[Int, A] =
      merge(newElement, threshold)

    /**
     * @return the next key
     */
    def nextKey: Int =
      if (map.isEmpty) {
        1
      } else {
        map.keys.max + 1
      }

    private def merge(newElement: A, threshold: Double): Map[Int, A] =
      recursiveMerge(newElement, threshold, map)

    @scala.annotation.tailrec
    private def recursiveMerge(newElement: A, threshold: Double, recMap: Map[Int, A]): Map[Int, A] =
      if (recMap.nonEmpty) {
        recMap.last._2.merge(newElement, threshold) match {
          case Some(x) if x.isInstanceOf[A] => map + (recMap.last._1 -> x.asInstanceOf[A])
          case None => recursiveMerge(newElement, threshold, recMap.take(recMap.size - 1))
        }
      } else {
        map + (nextKey -> newElement)
      }
  }

}
