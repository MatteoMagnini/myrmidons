package utility

import alice.tuprolog._
import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike

class PrologTest extends AnyWordSpecLike with BeforeAndAfter {

  val engine: Prolog = utility.prolog.Engine.initializeProlog

  "Prolog engine" when {

    val emptyTree = engine.toTerm("nil")
    val node1 = engine.toTerm("node(1,range(0,4),range(0,4))")
    val query1 = new Struct("insert",node1,emptyTree,new Var("R"))
    val solution1 = engine.toTerm("tree(nil,node(1,range(0,4),range(0,4)),nil)")

    "adding a node to an empty tree" should {
      "add it in the root" in {
        val result = engine.solve(query1)
        assert(solution1 == result.getTerm("R"))
      }
    }

    val node2 = engine.toTerm("node(2,range(2,6),range(8,12))")
    val query2 = new Struct("insert",node2,solution1,new Var("R"))
    "adding a second node" should {
      "add a level in the tree" in {
        val result = engine.solve(query2)
        println(result.getTerm("R"))
        //assert(solution1 == result.getTerm("R"))
      }
    }

  }

}
