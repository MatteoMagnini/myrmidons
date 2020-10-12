package utility

import alice.tuprolog._
import model.environment.pheromones.FoodPheromone
import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike
import utility.geometry.{Vector2D, ZeroVector2D}
import utility.prolog.RTree
import model.environment.pheromones.FoodPheromoneInfo._

class PrologTest extends AnyWordSpecLike with BeforeAndAfter {

  val engine: Prolog = utility.prolog.Engine.initializeProlog
  var rTree: RTree = RTree(engine)
  val decreasingFunction: scala.Double => scala.Double = x => x - DELTA

  "Prolog engine" when {

    val id1 = 1
    val pheromone1 = FoodPheromone(ZeroVector2D(), decreasingFunction, STARTING_INTENSITY)
    val solution1 = engine.toTerm("tree(nil,node(1,range(-10.0,10.0),range(-10.0,10.0)),nil)")

    "adding a node to an empty tree" should {
      "add it in the root" in {
        rTree = rTree.insert(id1,pheromone1)
        assert(solution1 == rTree.tree)
      }
    }

    val solution2 = engine.toTerm("tree(tree(nil,node(2,range(15.0,35.0),range(20.0,40.0)),nil)," +
      "node(none,range(-10.0,35.0),range(-10.0,40.0)),tree(nil,node(1,range(-10.0,10.0),range(-10.0,10.0)),nil))")
    val id2 = 2
    val pheromone2 = FoodPheromone(Vector2D(25,30), decreasingFunction, STARTING_INTENSITY)

    "adding a second node" should {
      "add a level in the tree" in {
        rTree = rTree.insert(id2,pheromone2)
        assert(solution2 == rTree.tree)
      }
    }

    val id3 = 3
    val pheromone3 = FoodPheromone(Vector2D(10,-10), decreasingFunction, STARTING_INTENSITY)

    "removing a not existing node" should {
      "return the same tree" in {
        rTree = rTree.remove(id3,pheromone3)
        val result = rTree.tree
        assert(solution2 == result)
      }
    }

    //TODO: not true now
    /*"removing an existing node" should {
      "go back to previous tree" in {
        rTree = rTree.remove(id2,pheromone2)
        val result = rTree.tree
        println(result)
        //assert(solution1 == result)
      }
    }*/

  }

}
