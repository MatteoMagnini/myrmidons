package common.rTree

import alice.tuprolog.{Prolog, Struct}
import common.geometry.Vector2D
import common.rTree.PrologFacilities._
import common.rTree.RTree.{Node, Range, Tree}
import model.environment.pheromones._

/** Scala-prolog integration: convert prolog functions and get results
  *
  * @param engine prolog engine
  */
class RTreeProlog(val engine: Prolog) {

  private val insert = "insert"
  private val remove = "removeWithFix"
  private val getLeavesList = "getLeavesList"
  private val query = "query"

  /** Inserts a node in a tree
    *
    * @param node node to be inserted
    * @param tree starting tree
    * @return tree with added node
    */
  def insertNode(node: Node[Int], tree: Tree[Int]): Tree[Int] = {
    val variable = Variable()
    val goal = new Struct(insert, node, tree, variable)
    val result = engine.solve(goal).getTerm(variable.getName)
    result getAsTree
  }

  /** Removes a node from a tree
    *
    * @param node node to be removed
    * @param tree starting tree
    * @return tree with removed node
    */
  def removeNode(node: Node[Int], tree: Tree[Int]): Tree[Int] = {
    val variable = Variable()
    val goal = new Struct(remove, node, tree, variable)
    val result = engine.solve(goal).getTerm(variable.getName)
    result getAsTree
  }

  /** Queries a tree over a certain range
    *
    * @param position center of query range
    * @param tree tree to be queried
    * @return list of leaves ids responding to query
    */
  def query(position: Vector2D, tree: Tree[Int]): List[Int] = {
    val variable = Variable()
    val ranges: (Range, Range) = position.rangeOfInfluence(INFLUENCE_RADIUS)
    val goal = new Struct(query, tree, ranges._1, ranges._2, variable)
    try {
      engine.solve(goal).getTerm(variable.getName).getAsList.map(_.getAsNode).map(_.id.get)
    } catch {
      case _:Exception => List.empty
    }
  }

  /** Get all leaves of tree
    *
    * @param tree input tree
    * @return list of tree leaves
    */
  def getLeaves(tree: Tree[Int]): Seq[Node[Int]] = {
    val variable = Variable()
    val goal = new Struct(getLeavesList, getTreeAsTerm(tree), variable)
    val result = engine.solve(goal).getTerm(variable.getName).getAsList
    result.map(_.getAsNode)
  }
}

object RTreeProlog {
  val PATH = "R-tree.pl"
  def apply(): RTreeProlog = new RTreeProlog(getEngine(PATH))
}
