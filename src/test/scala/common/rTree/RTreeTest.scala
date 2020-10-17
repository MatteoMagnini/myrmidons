package common.rTree

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import common.geometry.ZeroVector2D
import common.rTree.RTree.{MyRange, Node, Tree}

class RTreeTest extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  val engine = RTreeProlog()
  def mergedRanges(r1: MyRange, r2: MyRange): MyRange =
    (if (r1._1 <= r2._1) r1._1 else r2._1, if (r1._2 >= r2._2) r1._2 else r2._2)

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
    val node = Node(1, (0, 1), (0, 1))
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
        assert(result.root.get.rangeX == mergedRanges(node.rangeX, addedNode.rangeX))
        assert(result.root.get.rangeY == mergedRanges(node.rangeY, addedNode.rangeY))
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
    import common.geometry._
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

  "A general R-tree" when {
    var tree = Tree()
    val node1 = Node(1, (1,2), (3,4))
    val node2 = Node(2, (12,13), (15,16))
    val node3 = Node(3, (23,24), (21,22))
    val nodes = Seq(node1, node2, node3)
    tree = engine.insertNode(node1, tree)
    tree = engine.insertNode(node2, tree)
    tree = engine.insertNode(node3, tree)

    "adding a node" should {
      val addedNode = Node(4, (5,6), (7,8))
      val result = engine.insertNode(addedNode, tree)
      val leaves = engine.getLeaves(result)

      "contain correct number of nodes" in {
        assert(leaves.size == nodes.size + 1)
      }
      "contain all nodes" in {
        leaves.contains(addedNode)
        nodes.foreach(x => assert(leaves.contains(x)))
      }
      "have in root bigger range" in {
        assert(result.root.get.rangeX == mergedRanges(tree.root.get.rangeX, addedNode.rangeX))
        assert(result.root.get.rangeY == mergedRanges(tree.root.get.rangeY, addedNode.rangeY))
      }
    }
    "removing a node" should {
      val result = engine.removeNode(node1, tree)
      val leaves = engine.getLeaves(result)

      "contain correct number of nodes" in {
        assert(leaves.size == nodes.size - 1)
      }
      "not contain removed node" in {
        assert(!leaves.contains(node1))
      }
      "have in root fixed ranges" in {
        assert(result.root.get.rangeX == mergedRanges(node2.rangeX, node3.rangeX))
        assert(result.root.get.rangeY == mergedRanges(node2.rangeY, node3.rangeY))
      }
    }
    "queried" should {
      val queryPosition = (2,3)
      val result = engine.query(queryPosition, tree)

      "give correct number of nodes in output" in {
        assert(result.size == 1)
      }
      "give correct nodes as output" in {
        assert(result.contains(node1.id.get))
      }
    }
  }
}
