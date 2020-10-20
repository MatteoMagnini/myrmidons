package common.rTree

import common.geometry.Vector2D
import common.rTree.RTree.{Range, Tree}

object RTree {

  /** Define a new type, describing a range: lower bound coordinate and upper bound coordinate */
  type Range = (Double, Double)

  /** A node of r-tree
    *
    * @param id     identifier of node
    * @param rangeX range along x coordinate
    * @param rangeY range along y coordinate
    * @tparam A type of element to be put in tree (id field)
    */
  case class Node[A](id: Option[A], rangeX: Range, rangeY: Range) {

    /** Checks if two ranges in 2D space are intersected
      *
      * @param otherRanges ranges to be compared
      * @return whether ranges are intersected
      */
    def intersects(otherRanges: (Range, Range)): Boolean = {
      val otherRangeX = otherRanges._1
      val otherRangeY = otherRanges._2
      (rangeX._1 < otherRangeX._2) && (otherRangeX._1 < rangeX._2) &&
        (rangeY._1 < otherRangeY._2) && (otherRangeY._1 < rangeY._2)
    }
  }

  /** Factory methods to create a node: with or without defined id (for not leaves nodes) */
  object Node {
    def apply[A](id: Option[A], rangeX: Range, rangeY: Range): Node[A] = new Node(id, rangeX, rangeY)

    def apply[A](rangeX: Range, rangeY: Range): Node[A] = new Node[A](None, rangeX, rangeY)
  }

  /** Generic r-tree
    *
    * @tparam A type of values stored in r-tree
    */
  sealed trait Tree[A] {

    /** Left branch of tree */
    def left: Tree[A]

    /** Right branch of tree */
    def right: Tree[A]

    /** Root of tree */
    def root: Option[Node[A]]

    /** Size of tree */
    def size: Int
  }

  /** An implementation of r-tree */
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

    private def isLeaf(tree: Tree[A]): Boolean = tree match {
      case x: NotEmptyTree[A] => x.l.root.isEmpty || x.r.root.isEmpty
      case _ => false
    }
  }

  /** Empty tree */
  case class EmptyTree[A]() extends TreeImpl[A]

  /** Not-empty tree */
  case class NotEmptyTree[A](l: Tree[A], node: Node[A], r: Tree[A]) extends TreeImpl[A]

  /** Factory methods to create an r-tree */
  object Tree {
    def apply[A](): Tree[A] = EmptyTree()

    def apply[A](left: Tree[A], root: Node[A], right: Tree[A]): Tree[A] = NotEmptyTree(left, root, right)
  }

}

object ScalaEngine {

  /** Query over an r-tree
    *
    * @param position reference position in which query is done
    * @param tree     r-tree to be queried
    * @return list of leaves id that respond to query
    */
  def query(position: Vector2D, tree: Tree[Int]): Seq[Int] = {

    def _query(queriedRange: (Range, Range), tree: Tree[Int]): Seq[Int] = {
      tree.root match {
        case Some(r) => if (r intersects queriedRange) {
          r.id match {
            case Some(i) => i +: _query(queriedRange, tree.left) ++: _query(queriedRange, tree.right)
            case None => _query(queriedRange, tree.left) ++ _query(queriedRange, tree.right)
          }
        } else {
          Seq.empty
        }
        case None => Seq.empty
      }
    }

    _query(position.rangeOfInfluence(DEFAULT_RANGE), tree)
  }
}
