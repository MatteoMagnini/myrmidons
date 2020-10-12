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
      val node = Node(1, (0,0),(0,0))
      val prologResult = engine.insertNode(node, tree)

      "give as result a one-value tree" in {
        assert(prologResult.size == 1)
      }

      "have as root inserted node" in {
        assert(prologResult.root.get == node)
      }
    }
  }

  "A one-value tree" when {
    val tree = Tree(Tree(), Node(1, (1,0),(1,0)), Tree())

    "adding a node" should {
      val node = Node(2, (2,3), (2,3))
      val result = engine.insertNode(node, tree)

      "give as result a two-values tree" in {
        assert(result.size == 2)
      }

      "have as leaves initial and inserted nodes" in {

      }
    }
  }

}
