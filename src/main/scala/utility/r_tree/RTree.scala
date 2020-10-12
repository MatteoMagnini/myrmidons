package utility.r_tree

object RTree {

  type MyRange = (Double, Double)
  type Node = (Int, MyRange, MyRange)

  object Node {
    def apply(id: Int, rangeX: MyRange, rangeY: MyRange): Node = new Node(id, rangeX, rangeY)
  }

  sealed trait Tree {
    def left: Tree
    def right: Tree
    def root: Option[Node]
  }

  trait TreeImpl extends Tree {

    override def left: Tree = this match {
      case x: NonEmptyTree => x.l
      case  _ => EmptyTree()
    }

    override def right: Tree = this match {
      case x:NonEmptyTree => x.r
      case  _ => EmptyTree()
    }

    override def root: Option[Node] = this match {
      case x:NonEmptyTree => Some(x.node)
      case _ => None
    }
  }

  case class EmptyTree() extends TreeImpl
  case class NonEmptyTree(l: Tree, node: Node, r: Tree) extends TreeImpl

  object Tree {
    def apply(): Tree = EmptyTree()
    def apply(left: Tree, root: Node, right: Tree): Tree = NonEmptyTree(left, root, right)
  }
}
