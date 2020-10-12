package utility.r_tree


import PrologFacilities._
import alice.tuprolog.{Prolog, Struct, Term}
import utility.r_tree.RTree.{MyRange, Node, NonEmptyTree, Tree}

class RTreeProlog(val engine: Prolog) {

  val insert = "insert"

  def insertNode(node: Node, tree: Tree): Unit = {
    val goal = new Struct(insert, getNodeAsTerm(node), getTreeAsTerm(tree), Variable())
    println(goal)
    println(engine.solve(goal))
  }

  private def getNodeAsTerm(node: Node): Term =
    new Struct("node", TuPrologInt(node._1), getRangeAsTerm(node._2), getRangeAsTerm(node._3))
  private def getRangeAsTerm(range: MyRange): Term =
    new Struct("range", TuPrologDouble(range._1), TuPrologDouble(range._2))
  private def getTreeAsTerm(tree:Tree): Term = tree match {
    case x:NonEmptyTree => new Struct("tree", getTreeAsTerm(tree.left), getNodeAsTerm(tree.root.get), getTreeAsTerm(tree.right))
    case _ => new Struct("nil")
  }
}

object RTreeProlog {
  val PATH = "/R-tree.pl"
  def apply(): RTreeProlog = new RTreeProlog(getEngine(getClass.getResourceAsStream(PATH)))
}

object Main extends App {
  val engine = RTreeProlog()

  engine.insertNode(Node(2, (2,3), (2,3)), Tree(Tree(), Node(1, (1,2), (1,2)), Tree()) )
}