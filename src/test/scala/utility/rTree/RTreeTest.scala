package utility.rTree

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.rTree.RTree.{Node, Tree}

class RTreeTest extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  val engine = RTreeProlog()

  "An empty R-Tree" when {
    val tree = Tree()

    "adding a node" should {
      val node = Node(1, (0, 0), (0, 0))
      val prologResult = engine.insertNode(node, tree)

      "give as result a one-value tree" in {
        assert(prologResult.size == 1)
      }

      "have as root inserted node" in {
        assert(prologResult.root.get == node)
      }
    }

    "removing a node" should {
      val node = Node(1, (0, 0), (0, 0))
      val prologResult = engine.removeNode(node, tree)

      "give an empty tree as result" in {
        assert(prologResult.size == 0)
      }
    }
  }

  "A one-value tree" when {
    val node = Node(1, (1, 0), (1, 0))
    val tree = Tree(Tree(), node, Tree())

    "adding a node" should {
      val addedNode = Node(2, (2, 3), (2, 3))
      val result = engine.insertNode(addedNode, tree)

      "give as result a two-values tree" in {
        assert(result.size == 2)
      }
      "have as leaves initial and inserted nodes" in {
        val result2 = engine.getLeaves(result)
        assert(result2.contains(node))
        assert(result2.contains(addedNode))
      }
      "have as root merged ranges" in {
        val mergedRange = ((1, 3), (1, 3))
        assert(result.root.get.rangeX == mergedRange._1)
        assert(result.root.get.rangeY == mergedRange._2)
      }
    }
    "removing node" should {
      val result = engine.removeNode(node, tree)
      "give as result an empty tree" in {
        assert(result.size == 0)
      }
    }
  }

}
