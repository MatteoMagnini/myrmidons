package utility.rTree

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import utility.geometry.{Vector2D, ZeroVector2D}
import utility.rTree.RTree.{Node, Tree}

class RTreeTest extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  val engine = RTreeProlog()

  "An empty R-Tree" when {
    val tree = Tree()
    val node = Node(1, (0, 0), (0, 0))

    "adding a node" should {
      val prologResult = engine.insertNode(node, tree)

      "give as result a one-value tree" in {
        assert(prologResult.size == 1)
      }

      "have as root inserted node" in {
        assert(prologResult.root.get == node)
      }
    }

    "removing a node" should {
      val prologResult = engine.removeNode(node, tree)

      "give an empty tree as result" in {
        assert(prologResult.size == 0)
      }
    }

    "queried" should {
      val prologResult = engine.query(ZeroVector2D(), tree)

      "give an empty tree as result" in {
        assert(prologResult.isEmpty)
      }
    }
  }

  "A one-value tree" when {
    val node = Node(1, (1, 0), (1, 0))
    val otherNode = Node(2, (2, 3), (2, 3))
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
    "removing a non present node" should {
      val result  = engine.removeNode(otherNode, tree)
      "give as result initial tree" in {
        assert(result == tree)
      }
    }
    import utility.geometry._
    "queried in a position inside R-tree ranges" should {
      val position = (1,1)
      val result = engine.query(position, tree)
      "give node as result" in {
        assert(result.length == 1)
        assert(result.contains(node.id.get))
      }
    }
    "queried in a position outside R-tree range" should {
      val position = (100,100)
      val result = engine.query(position, tree)
      "give node as result" in {
        assert(result.isEmpty)
      }
    }
  }

}
