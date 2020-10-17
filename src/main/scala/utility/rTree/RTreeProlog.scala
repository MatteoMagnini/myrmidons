package utility.rTree

import alice.tuprolog.{Prolog, Struct}
import model.environment.pheromones._
import utility.geometry.Vector2D
import utility.rTree.PrologFacilities._
import utility.rTree.RTree.{MyRange, Node, Tree}

class RTreeProlog(val engine: Prolog) {

  private val insert = "insert"
  private val remove = "removeWithFix"
  private val getLeavesList = "getLeavesList"
  private val query = "queryToList"

  def insertNode(node: Node, tree: Tree): Tree = {
    val variable = Variable()
    val goal = new Struct(insert, node, tree, variable)
    val result = engine.solve(goal).getTerm(variable.getName)
    result getAsTree
  }

  def removeNode(node: Node, tree: Tree): Tree = {
    val variable = Variable()
    val goal = new Struct(remove, node, tree, variable)
    println("GOAL: " + goal)
    val result = engine.solve(goal).getTerm(variable.getName)
    println("RESULT: " + result)
    result getAsTree
  }

  def query(position: Vector2D, tree: Tree): List[Int] = {
    val variable = Variable()
    val ranges: (MyRange, MyRange) = position.rangeOfInfluence(INFLUENCE_RADIUS)
    val goal = new Struct(query, tree, ranges._1, ranges._2, variable)
    try {
      engine.solve(goal).getTerm(variable.getName).getAsList.map(_.getAsNode).map(_.id.get)
    } catch {
      case _:Exception => List.empty
    }
  }

  def getLeaves(tree: Tree): Seq[Node] = {
    val variable = Variable()
    val goal = new Struct(getLeavesList, getTreeAsTerm(tree), variable)
    println(goal)
    val result = engine.solve(goal).getTerm(variable.getName).getAsList
    result.map(_.getAsNode)
  }
}

object RTreeProlog {
  val PATH = "R-tree.pl"

  def apply(): RTreeProlog = new RTreeProlog(getEngine(PATH))
}
