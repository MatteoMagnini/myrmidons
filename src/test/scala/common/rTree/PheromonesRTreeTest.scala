package common.rTree

import model.environment.pheromones.FoodPheromone
import model.environment.pheromones.FoodPheromoneInfo.{STARTING_INTENSITY, _}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import common.geometry.{Vector2D, ZeroVector2D}
import common.rTree.RTree.{Node, Tree}

class PheromonesRTreeTest extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  val engine = RTreeProlog()
  val decreasingFunction: scala.Double => scala.Double = x => x - DELTA

  "Prolog engine" when {

    val id1 = 1
    val pheromone1 = FoodPheromone(ZeroVector2D(), decreasingFunction, STARTING_INTENSITY)
    val initialRTree: Tree[Int] = Tree()
    var oneLeafTree: Tree[Int]  = Tree()
    var twoLeavesTree: Tree[Int]  = Tree()

    "adding a node to an empty tree" should {
      oneLeafTree = engine.insertNode((id1, pheromone1), initialRTree)

      "add it in the root" in {
        assert(implicitly[Node[Int]]((id1, pheromone1)) == oneLeafTree.root.get)
      }
    }

    val id2 = 2
    val pheromone2 = FoodPheromone(Vector2D(25, 30), decreasingFunction, STARTING_INTENSITY)

    "adding a second node" should {
      twoLeavesTree = engine.insertNode((id2, pheromone2), oneLeafTree)

      "add a level in the tree" in {
        val leaves: Seq[Node[Int]] = engine.getLeaves(twoLeavesTree)
        assert(leaves.contains(implicitly[Node[Int]]((id1, pheromone1))))
        assert(leaves.contains(implicitly[Node[Int]]((id2, pheromone2))))
      }
    }

    "searching near node" should {
      val searchedRange = Vector2D(10, 10)
      val queryResult = engine.query(searchedRange, twoLeavesTree)

      "return correct number of ids" in {
        assert(queryResult.size == 1)
      }
      "return correct pheromones ids" in {
        assert(queryResult.contains(id1))
      }
    }

    val id3 = 3
    val pheromone3 = FoodPheromone(Vector2D(10, -10), decreasingFunction, STARTING_INTENSITY)

    "removing a not existing node" should {
      val newRTree = engine.removeNode((id3, pheromone3), twoLeavesTree)

      "return the same tree" in {
        assert(newRTree == twoLeavesTree)
      }
    }

    "removing an existing node" should {
      "go back to previous tree" in {
        val resultTree = engine.removeNode((id2, pheromone2), twoLeavesTree)
        assert(resultTree == oneLeafTree)
      }
    }
  }

}
