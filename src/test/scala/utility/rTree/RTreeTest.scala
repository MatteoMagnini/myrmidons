package utility.rTree

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.rTree.RTree.{Node, Tree}

class RtreeTest extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  val engine = RTreeProlog()

  "An empty R-Tree" when {
    val tree = Tree()

    "adding a node" should {
      val node = Node(Some(1), (0,0),(0,0))

      "give as result a one-value tree" in {
        val prologResult = engine.insertNode(node, tree)
        assert(prologResult.size == 1)
      }
    }
  }

}
