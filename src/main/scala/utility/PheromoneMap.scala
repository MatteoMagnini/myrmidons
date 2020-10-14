package utility

import model.environment.pheromones.Pheromone
import utility.geometry.{Vector2D, ZeroVector2D}

object PheromoneMap {

  implicit class PheromoneMap[A <: Pheromone](map: Map[Int, A]){

    def tick(): Map[Int, A] =
      map.toStream.map(m => m._1 -> m._2.decrease)
        .filter(opt => opt._2.isDefined)
        .map(opt => opt._1 -> opt._2.get.asInstanceOf[A]).toMap

    def add(newElement: A, threshold: Double = 1E-10): Map[Int, A] =
      merge(newElement,threshold)

    def nextKey: Int =
      if(map.isEmpty) {
        1
      } else {
        map.keys.max + 1
      }

    def strongest: Option[A] =
      if (map.isEmpty) None else Some(map.values.toStream.sortWith((e1, e2) => e1.intensity > e2.intensity).last)

    def weightedSum(position: Vector2D): Vector2D =
      if (map.isEmpty) {
        ZeroVector2D()
      }
      else {
        map.values.toStream.map(e => (e.position - position) * (e.intensity / (e.position --> position))).reduce(_ >> _)
      }

    private def merge(newElement: A, threshold: Double): Map[Int,A] =
      recursiveMerge(newElement, threshold, map)

    @scala.annotation.tailrec
    private def recursiveMerge(newElement: A, threshold: Double, recMap: Map[Int,A]): Map[Int,A] =
      if (recMap.nonEmpty) {
        recMap.last._2.merge(newElement,threshold) match {
          case Some(x) if x.isInstanceOf[A] => map + (recMap.last._1 -> x.asInstanceOf[A])
          case None => recursiveMerge(newElement, threshold, recMap.take(recMap.size - 1))
        }
      } else {
        map + (nextKey -> newElement)
      }
  }
}
