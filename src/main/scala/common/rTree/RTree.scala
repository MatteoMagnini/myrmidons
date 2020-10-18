package common.rTree

import common.geometry.Vector2D
import common.rTree.RTree.Tree

object RTree {

  type MyRange = (Double, Double)
  case class Node(id: Option[Int], rangeX: MyRange, rangeY: MyRange){
    def intersects(ranges: (MyRange, MyRange)): Boolean = {
      val rangeX = ranges._1
      val rangeY = ranges._2
      (this.rangeX._1 <= rangeX._2) && (rangeX._1 <= this.rangeX._2) &&
        (this.rangeY._1 <= rangeY._2) && (this.rangeY._1 <= rangeY._2)
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
    def height: Int
  }

  trait TreeImpl extends Tree {

    override def left: Tree = this match {
      case x: NotEmptyTree => x.l
      case _ => EmptyTree()
    }

    override def right: Tree = this match {
      case x: NotEmptyTree => x.r
      case _ => EmptyTree()
    }

    override def root: Option[Node] = this match {
      case x: NotEmptyTree => Some(x.node)
      case _ => None
    }

    override def size: Int = this match {
      case x: NotEmptyTree if isLeaf(x) => 1 + x.l.size + x.r.size
      case x: NotEmptyTree => x.l.size + x.r.size
      case _ => 0
    }

    override def height: Int = this match {
      case x:NotEmptyTree => 1 + x.l.height
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

object ScalaEngine {

  def query(position: Vector2D,tree: Tree): Seq[Int] = {
    tree.root match {
      case Some(r) => if(r intersects position.rangeOfInfluence(DEFAULT_RANGE)){
        val id = r.id match {
          case Some(i) => Seq(i)
          case None => Seq.empty
        }
        id ++ query(position, tree.left) ++ query(position, tree.right)
      } else {
        Seq.empty
      }
      case None => Seq.empty
    }
  }

}