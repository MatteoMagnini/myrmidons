package common.rTree

import common.geometry.Vector2D
import common.rTree.RTree.Tree

object RTree {

  type MyRange = (Double, Double)

  case class Node[A](id: Option[A], rangeX: MyRange, rangeY: MyRange) {

    def intersects(ranges: (MyRange, MyRange)): Boolean = {
      val rangeX = ranges._1
      val rangeY = ranges._2
      (this.rangeX._1 <= rangeX._2) && (rangeX._1 <= this.rangeX._2) &&
        (this.rangeY._1 <= rangeY._2) && (this.rangeY._1 <= rangeY._2)
    }
  }

  object Node {
    def apply[A](id: Option[A], rangeX: MyRange, rangeY: MyRange): Node[A] = new Node(id, rangeX, rangeY)

    def apply[A](rangeX: MyRange, rangeY: MyRange): Node[A] = new Node[A](None, rangeX, rangeY)
  }

  sealed trait Tree[A] {

    def left: Tree[A]

    def right: Tree[A]

    def root: Option[Node[A]]

    def size: Int

    def height: Int
  }

  trait TreeImpl[A] extends Tree[A] {

    override def left: Tree[A] = this match {
      case x: NotEmptyTree[A] => x.l
      case _ => EmptyTree()
    }

    override def right: Tree[A] = this match {
      case x: NotEmptyTree[A] => x.r
      case _ => EmptyTree()
    }

    override def root: Option[Node[A]] = this match {
      case x: NotEmptyTree[A] => Some(x.node)
      case _ => None
    }

    override def size: Int = this match {
      case x: NotEmptyTree[A] if isLeaf(x) => 1 + x.l.size + x.r.size
      case x: NotEmptyTree[A] => x.l.size + x.r.size
      case _ => 0
    }

    override def height: Int = this match {
      case x: NotEmptyTree[A] => 1 + x.l.height
      case _ => 0
    }

    private def isLeaf(tree: Tree[A]): Boolean = tree match {
      case x: NotEmptyTree[A] => x.l.root.isEmpty || x.r.root.isEmpty
      case _ => false
    }

  }

  case class EmptyTree[A]() extends TreeImpl[A]

  case class NotEmptyTree[A](l: Tree[A], node: Node[A], r: Tree[A]) extends TreeImpl[A]

  object Tree {
    def apply[A](): Tree[A] = EmptyTree()

    def apply[A](left: Tree[A], root: Node[A], right: Tree[A]): Tree[A] = NotEmptyTree(left, root, right)
  }

}

object ScalaEngine {

  def query(position: Vector2D, tree: Tree[Int]): Seq[Int] = {
    tree.root match {
      case Some(r) => if (r intersects position.rangeOfInfluence(DEFAULT_RANGE)) {
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