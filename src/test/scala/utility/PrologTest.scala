package utility

import alice.tuprolog._
import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike

class PrologTest extends AnyWordSpecLike with BeforeAndAfter {

  val engine: Prolog = utility.prolog.Engine.initializeProlog

  "Prolog engine" when {

    "adding a node to an empty tree" should {

      val emptyTree = engine.toTerm("nil")
      val node = engine.toTerm("node(1,range(0,4),range(0,4))")
      val query = new Struct("insert",node,emptyTree,new Var("R"))
      val result = engine.solve(query)
      val solution = engine.toTerm("tree(nil,node(1,range(0,4),range(0,4)),nil)")

      "add it in the root" in {

        assert(solution == result.getTerm("R"))
      }
    }

    "adding a second node" should {

      "add a level in the tree" in {


      }
    }

  }

}
