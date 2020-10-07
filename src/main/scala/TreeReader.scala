object TreeReader extends App {
    val tree = VTree(VTree(EmptyTree(), Node(Range(5,5), Range(1,5)), EmptyTree()), Node(Range(1,3), Range(1,2)), EmptyTree())
    print(tree.draw)
}

case class Range(x1:Int, x2:Int)
case class Node(x:Range, y:Range)

sealed trait Tree {
  def draw:String
}
case class VTree(left: Tree, node: Node, right: Tree) extends Tree{
  override def draw: String = node.toString + "\n" + left.draw + " " + right.draw
}
case class EmptyTree() extends Tree{
  override def draw: String = "Empty"
}


// tree(tree(tree(nil,node(range(2,3),range(7,8)),nil),node(range(2,4),range(5,8)),tree(nil,node(range(3,4),range(5,6)),nil)),node(range(1,7),range(1,8)),tree(tree(nil,node(range(6,7),range(3,4)),nil),node(range(1,7),range(1,4)),tree(nil,node(range(1,2),range(1,2)),nil)))