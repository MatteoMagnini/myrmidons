package common.rTree

import model.environment.pheromones.INFLUENCE_RADIUS
import common.geometry.Vector2D

object RTree {

  type MyRange = (Double, Double)
  case class Node(id: Option[Int], rangeX: MyRange, rangeY: MyRange){
    def intersects(range: MyRange, rangeY: MyRange): Boolean = {
      val intersectionRangeX = (math.max(this.rangeX._1, rangeX._1), math.min(this.rangeX._2, rangeX._2))
      val intersectionRangeY = (math.max(this.rangeY._1, rangeY._1), math.min(this.rangeY._2, rangeY._2))
      (intersectionRangeX._1 < intersectionRangeX._2) && (intersectionRangeY._1 < intersectionRangeY._2)
    }
  }

  object Node {
    def apply(id: Option[Int], rangeX: MyRange, rangeY: MyRange): Node = new Node(id, rangeX, rangeY)
    def apply(rangeX: MyRange, rangeY: MyRange): Node = new Node(None, rangeX, rangeY)
  }

  sealed trait Tree {
    def left: Tree
    def right: Tree
    def root: Option[Node]
    def size: Int
  }

  trait TreeImpl extends Tree {

    override def left: Tree = this match {
      case x: NotEmptyTree => x.l
      case  _ => EmptyTree()
    }

    override def right: Tree = this match {
      case x:NotEmptyTree => x.r
      case  _ => EmptyTree()
    }

    override def root: Option[Node] = this match {
      case x:NotEmptyTree => Some(x.node)
      case _ => None
    }

    override def size: Int = this match {
      case x:NotEmptyTree if isLeaf(x) => 1 + x.l.size + x.r.size
      case x:NotEmptyTree => x.l.size + x.r.size
      case _ => 0
    }

    private def isLeaf(tree:Tree):Boolean = tree match {
      case x:NotEmptyTree => x.l.root.isEmpty || x.r.root.isEmpty
      case _ => false
    }

  }

  case class EmptyTree() extends TreeImpl
  case class NotEmptyTree(l: Tree, node: Node, r: Tree) extends TreeImpl

  object Tree {
    def apply(): Tree = EmptyTree()
    def apply(left: Tree, root: Node, right: Tree): Tree = NotEmptyTree(left, root, right)
  }
}
