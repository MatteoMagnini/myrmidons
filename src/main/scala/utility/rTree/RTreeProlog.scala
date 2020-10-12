package utility.rTree

import PrologFacilities._
import alice.tuprolog.{Prolog, Struct, Term}
import utility.rTree.RTree.{MyRange, Node, NotEmptyTree, Tree}

class RTreeProlog(val engine: Prolog) {

  val insert = "insert"
  val range = "range"
  val node = "node"
  val tree = "tree"
  val nil = "nil"

  def insertNode(node: Node, tree: Tree): Tree = {
    val variable = Variable()
    val goal = new Struct(insert, getNodeAsTerm(node), getTreeAsTerm(tree), variable)
    val result = engine.solve(goal).getTerm(variable.getName)
    getTermAsTree(result)
  }

  def getNodeAsTerm(n: Node): Term =
    new Struct(node, TuPrologInt(n._1.get), getRangeAsTerm(n._2), getRangeAsTerm(n._3))

  def getTermAsNode(term: Term): Node = {
    val struct = term.getTerm.asInstanceOf[Struct]
    Node(struct.getArg(0).getAsInt, getTermAsRange(struct.getArg(1)), getTermAsRange(struct.getArg(2)))

  }

  def getRangeAsTerm(r: MyRange): Term =
    new Struct(range, TuPrologDouble(r._1), TuPrologDouble(r._2))

  def getTermAsRange(term: Term): (Double, Double) = {
    val struct = term.getTerm.asInstanceOf[Struct]
    (struct.getArg(0).toString.toDouble, struct.getArg(1).toString.toDouble)
  }

  def getTreeAsTerm(t: Tree): Term = t match {
    case x: NotEmptyTree => new Struct(tree, getTreeAsTerm(t.left), getNodeAsTerm(t.root.get), getTreeAsTerm(t.right))
    case _ => Term.createTerm(nil)
  }

  def getTermAsTree(term: Term): Tree = term match {
    case x if x.isCompound =>
      val struct = x.getTerm.asInstanceOf[Struct]
      val root = getTermAsNode(struct.getArg(1))
      Tree(getTermAsTree(struct.getArg(0)), root, getTermAsTree(struct.getArg(2)))
    case _ => Tree()
  }
}

object RTreeProlog {
  val PATH = "/R-tree.pl"

  def apply(): RTreeProlog = new RTreeProlog(getEngine(getClass.getResourceAsStream(PATH)))
}

object Main extends App {
  val engine = RTreeProlog()
  val res = engine.insertNode(Node(Some(2), (2, 3), (2, 3)), Tree(Tree(), Node(Some(1), (1, 2), (1, 2)), Tree()))
  println(res)
  val struct = engine.getTreeAsTerm(Tree(Tree(), Node(Some(1), (1, 2), (1, 2)), Tree()))
  val m = struct.asInstanceOf[Struct]
  val tree = engine.getTermAsTree(m)
  println(tree)
  val t = engine.getTreeAsTerm(tree)


}