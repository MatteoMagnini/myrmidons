package common

import alice.tuprolog.{Struct, Term}
import common.geometry.Vector2D
import common.rTree.PrologFacilities.{TuPrologDouble, TuPrologInt}
import common.rTree.RTree.{Node, NotEmptyTree, Range, Tree}
import model.environment.pheromones._

package object rTree {

  val range = "range"
  val node = "node"
  val tree = "tree"
  val nil = "nil"
  val none = "none"

  /** Pimping prolog Term to extract tree instances
   *
   * @param term prolog term
   */
  implicit class RichTerm(term: Term) {

    /** Conversion from term to integer */
    def getAsInt: Option[Int] =
      try {
        Some(term.toString.toInt)
      } catch {
        case _: Exception => None
      }

    /** Conversion from term to [[Node]]*/
    def getAsNode: Node[Int] = {
      val struct = term.getTerm.asInstanceOf[Struct]
      Node(struct.getArg(0).getAsInt, struct.getArg(1) getAsRange, struct.getArg(2) getAsRange)
    }

    /** Conversion from term to [[Range]]*/
    def getAsRange: (Double, Double) = {
      val struct = term.getTerm.asInstanceOf[Struct]
      (struct.getArg(0).toString.toDouble, struct.getArg(1).toString.toDouble)
    }

    /** Conversion from term to [[Tree]]*/
    def getAsTree: Tree[Int] = term match {
      case x if x.isCompound =>
        val struct = x.getTerm.asInstanceOf[Struct]
        val root = struct.getArg(1).getAsNode
        Tree(struct.getArg(0) getAsTree, root, struct.getArg(2) getAsTree)
      case _ => Tree()
    }

    /** Conversion from term to list of terms */
    def getAsList: List[Term] = term match {
      case x if x.isCompound =>
        val struct = x.getTerm.asInstanceOf[Struct]
        struct.listHead() :: struct.listTail().getAsList
      case _ => Nil
    }
  }

  /** Conversion from node to prolog term */
  implicit def getNodeAsTerm(n: Node[Int]): Term = n.id match {
    case Some(_) => new Struct(node, TuPrologInt(n.id.get), n.rangeX, n.rangeY)
    case _ => new Struct(node, Term.createTerm(none), n.rangeX, n.rangeY)
  }

  /** Conversion from range to prolog term */
  implicit def getRangeAsTerm(r: Range): Term =
    new Struct(range, TuPrologDouble(r._1), TuPrologDouble(r._2))

  /** Conversion from tree to prolog term */
  implicit def getTreeAsTerm(t: Tree[Int]): Term = t match {
    case x: NotEmptyTree[Int] => new Struct(tree, getTreeAsTerm(t.left), t.root.get, getTreeAsTerm(t.right))
    case _ => Term.createTerm(nil)
  }

  /** Conversion from pheromone with id to prolog term */
  implicit def getPheromoneAsNode(p: (Int, Pheromone)): Node[Int] = {
    val ranges = p._2.position.rangeOfInfluence(INFLUENCE_RADIUS)
    Node(p._1, ranges._1, ranges._2)
  }

  implicit def toOption[X](value: X): Option[X] = Some(value)

  /** Pimping [[Vector2D]] to have a function to create a range from a position
   *
   * @param position input position
   */
  implicit class RichPosition(position: Vector2D) {

    /**
     * @param influence width of created range
     * @return range around position
     */
    def rangeOfInfluence(influence: Double): (Range, Range) = {
      ((position.x - influence, position.x + influence), (position.y - influence, position.y + influence))
    }
  }

}
